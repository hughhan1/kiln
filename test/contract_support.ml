module Error = Kiln_core.Error

let check label condition = if not condition then failwith label

let ok = function
  | Ok value -> value
  | Error error -> failwith (Error.show error)

let contains text pattern = Option.is_some (String.find_first ~sub:pattern text)

let ordered steps prefixes =
  let indices =
    List.map
      (fun prefix ->
        match List.find_index (String.starts_with ~prefix) steps with
        | Some index -> index
        | None -> failwith ("missing step: " ^ prefix))
      prefixes
  in
  indices = List.sort_uniq Int.compare indices

let lists_supported result supported =
  match result with
  | Ok _ -> false
  | Error error ->
      contains (Error.show error)
        ("; supported: " ^ String.concat ", " supported)

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
  Yojson.Basic.to_string (`Assoc (("source", `String root) :: extras))

let prepare ?providers root fields =
  Kiln_io.Driver.prepare ?providers (request root fields)

let package prepared name =
  Yojson.Basic.Util.(
    `Assoc (Kiln_planning.Planner.info prepared)
    |> member "resolvedPackages" |> member name)

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
let last values = List.nth values (List.length values - 1)
