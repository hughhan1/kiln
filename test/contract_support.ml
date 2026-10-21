module Error = Kiln_core.Error

let check label condition = if not condition then failwith label

let ok = function
  | Ok value -> value
  | Error error -> failwith (Error.show error)

let contains text pattern = Option.is_some (String.find_first ~sub:pattern text)

let registry_index =
  {|{"manifests":[{"platform":{"os":"linux","architecture":"amd64"}},{"platform":{"os":"linux","architecture":"arm64"}}]}|}

let registry repositories =
  let responses =
    List.concat_map
      (fun (repository, tags) ->
        let root = "https://registry-1.docker.io/v2/" ^ repository ^ "/" in
        ( "https://auth.docker.io/token?service=registry.docker.io&scope=repository:"
          ^ repository ^ ":pull",
          (None, {|{"token":"test"}|}) )
        :: ( root ^ "tags/list",
             ( Some "test",
               Yojson.Basic.to_string
                 (`Assoc
                    [ ("tags", `List (List.map (fun tag -> `String tag) tags)) ])
             ) )
        :: List.map
             (fun tag ->
               (root ^ "manifests/" ^ tag, (Some "test", registry_index)))
             tags)
      repositories
  in
  fun url token ->
    match List.assoc_opt url responses with
    | Some (expected, bytes) ->
        check ("registry authorization: " ^ url) (token = expected);
        Ok bytes
    | None -> failwith ("unexpected registry URL: " ^ url)

let rec remove path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter
        (fun entry -> remove (Filename.concat path entry))
        (Sys.readdir path);
      Unix.rmdir path
  | Unix.S_REG | Unix.S_LNK | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO
  | Unix.S_SOCK ->
      Sys.remove path

let write root path bytes =
  Out_channel.with_open_bin (Filename.concat root path) (fun channel ->
      Out_channel.output_string channel bytes)

let request root extras =
  let defaults =
    [
      ("source", `String root);
      ("platforms", `List [ `String "linux/amd64"; `String "linux/arm64" ]);
    ]
  in
  Yojson.Basic.to_string
    (`Assoc
       (extras
       @ List.filter (fun (key, _) -> not (List.mem_assoc key extras)) defaults
       ))

let failed = ref false

let test label action =
  try
    action ();
    Printf.printf "%s: passed\n%!" label
  with exn ->
    failed := true;
    Printf.eprintf "%s: %s\n%!" label (Printexc.to_string exn)

let temporary action =
  let root = Filename.temp_file "kiln-contract" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> remove root) (fun () -> action root)

let read path = In_channel.with_open_bin path In_channel.input_all

let environment overrides =
  Array.of_list
    (List.map (fun (key, value) -> key ^ "=" ^ value) overrides
    @ List.filter
        (fun entry ->
          not
            (List.exists
               (fun (key, _) -> String.starts_with ~prefix:(key ^ "=") entry)
               overrides))
        (Array.to_list (Unix.environment ())))

let run ?(env = Unix.environment ()) ?(exit_code = 0) ?errors executable
    arguments =
  let args = executable :: arguments in
  temporary (fun root ->
      let stdout = Filename.concat root "stdout" in
      let stderr = Filename.concat root "stderr" in
      let pid =
        Out_channel.with_open_bin stdout (fun out ->
            Out_channel.with_open_bin stderr (fun err ->
                Unix.create_process_env executable (Array.of_list args) env
                  Unix.stdin
                  (Unix.descr_of_out_channel out)
                  (Unix.descr_of_out_channel err)))
      in
      let deadline = Unix.gettimeofday () +. 600. in
      let rec wait () =
        match Unix.waitpid [ Unix.WNOHANG ] pid with
        | 0, _ ->
            if Unix.gettimeofday () >= deadline then
              failwith ("command timed out: " ^ String.concat " " args);
            ignore (Unix.select [] [] [] 0.05);
            wait ()
        | _, status -> status
      in
      let status =
        try wait ()
        with exn ->
          (try Unix.kill pid Sys.sigkill
           with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
          ignore (Unix.waitpid [] pid);
          raise exn
      in
      Option.iter (fun buffer -> Buffer.add_string buffer (read stderr)) errors;
      check
        (String.concat " " args ^ "\n" ^ read stdout ^ read stderr)
        (status = Unix.WEXITED exit_code);
      read stdout)

let snapshot files label action =
  test label (fun () ->
      temporary (fun root ->
          List.iter (fun (path, bytes) -> write root path bytes) files;
          action root))

let finish () = if !failed then exit 1
