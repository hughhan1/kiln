let fail ?(code = 1) message =
  prerr_endline message;
  exit code

let write path contents =
  Kiln_io.Path.mkdir ~perm:0o755 (Filename.dirname path);
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel contents)

let request directory provider start build_cmd start_cmd config_file previous
    error_missing_start entries platforms =
  let environment =
    match Kiln_planning.Environment.of_args ~getenv:Sys.getenv_opt entries with
    | Ok environment -> environment
    | Error error -> fail (Kiln_core.Error.show error)
  in
  let directory = Unix.realpath directory in
  let previous =
    List.map (fun (name, version) -> (name, `String version)) previous
  in
  let fields =
    [
      ("previousVersions", `Assoc previous);
      ("source", `String directory);
      ("errorMissingStartCommand", `Bool error_missing_start);
      ("environment", Kiln_planning.Environment.encode environment);
      ("buildCommand", `String build_cmd);
      ("startCommand", `String start_cmd);
      ("configFilePath", `String config_file);
    ]
    @ List.filter_map Fun.id
        [
          Option.map
            (fun values ->
              ("platforms", Kiln_planning.Json.string_array values))
            platforms;
          Option.map (fun value -> ("provider", `String value)) provider;
          (if start = [] then None
           else
             Some ("start", `List (List.map (fun value -> `String value) start)));
        ]
  in
  (directory, fields)

let prepare (directory, fields) plan_out info_out show_plan hide_pretty =
  (match (plan_out, info_out) with
  | Some plan, Some info
    when Kiln_io.Path.absolute plan = Kiln_io.Path.absolute info ->
      fail "plan-out and info-out must be different files"
  | _ -> ());
  let info fields =
    Option.iter
      (fun path -> write path (Yojson.Basic.to_string (`Assoc fields) ^ "\n"))
      info_out
  in
  match Kiln_io.Driver.prepare (Yojson.Basic.to_string (`Assoc fields)) with
  | Error error ->
      let message = Kiln_core.Error.show error in
      info (Kiln_planning.Planner.failure error);
      fail ~code:(Kiln_core.Error.exit_status error) message
  | Ok prepared ->
      let dockerfile = Kiln_planning.Planner.dockerfile prepared in
      Option.iter (fun path -> write path dockerfile) plan_out;
      info (Kiln_planning.Planner.info prepared);
      List.iter prerr_endline (Kiln_planning.Planner.warnings prepared);
      if not hide_pretty then Printf.printf "Prepared %s\n" directory;
      if show_plan then print_string dockerfile

let main () =
  let module Arg = Cmdliner.Arg in
  let module Cmd = Cmdliner.Cmd in
  let module Term = Cmdliner.Term in
  let nonempty =
    Arg.Conv.make ~docv:"PATH"
      ~parser:(function "" -> Error "must not be empty" | s -> Ok s)
      ~pp:Format.pp_print_string ()
  in
  let directory =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"DIRECTORY")
  in
  let option converter default name doc =
    Arg.(last & opt_all converter [ default ] & info [ name ] ~doc)
  in
  let path name doc = option Arg.(some nonempty) None name doc in
  let flag name doc = option Arg.bool false name doc in
  let provider =
    option Arg.(some string) None "provider" "Select an application provider."
  in
  let start =
    Arg.(
      value & opt_all string []
      & info [ "start-arg" ] ~docv:"ARG"
          ~doc:
            "Startup argv element; repeat in order, starting with the \
             executable.")
  in
  let build_cmd =
    option Arg.string "" "build-cmd" "Override the build shell command."
  in
  let start_cmd =
    option Arg.string "" "start-cmd" "Override the startup shell command."
  in
  let config_file =
    option Arg.string "railpack.json" "config-file"
      "Application configuration file."
  in
  let previous =
    Arg.(
      value
      & opt_all (pair ~sep:'@' nonempty nonempty) []
      & info [ "previous" ] ~docv:"PACKAGE@VERSION"
          ~doc:"Previously resolved package@version; repeat per package.")
  in
  let application_env =
    Arg.(
      value & opt_all string []
      & info [ "e"; "env" ] ~docv:"NAME[=VALUE]"
          ~doc:
            "Provide an application build variable; bare names inherit from \
             the process environment.")
  in
  let request =
    Term.(
      const request $ directory $ provider $ start $ build_cmd $ start_cmd
      $ config_file $ previous
      $ flag "error-missing-start" "Require a startup command."
      $ application_env
      $ option
          Arg.(some (list string))
          None "platform"
          "Comma-separated Linux targets; defaults to the host architecture.")
  in
  let prepare =
    Cmd.v
      (Cmd.info "prepare" ~doc:"Prepare an application's Dockerfile")
      Term.(
        const prepare $ request
        $ path "plan-out" "Write the Dockerfile."
        $ path "info-out" "Write JSON result metadata."
        $ flag "show-plan" "Print the Dockerfile."
        $ flag "hide-pretty-plan" "Suppress the preparation summary.")
  in
  let rec normalize_args = function
    | "--" :: _ as args -> args
    | "-h" :: rest -> "--help" :: rest
    | "-e" :: rest -> normalize_args ("--env" :: rest)
    | (("--show-plan" | "--hide-pretty-plan" | "--error-missing-start") as flag)
      :: rest ->
        (flag ^ "=true") :: normalize_args rest
    | (( "--platform" | "--plan-out" | "--info-out" | "--provider"
       | "--start-arg" | "--build-cmd" | "--start-cmd" | "--config-file"
       | "--previous" | "--env" ) as option)
      :: value :: rest ->
        (option ^ "=" ^ value) :: normalize_args rest
    | arg :: rest -> arg :: normalize_args rest
    | [] -> []
  in
  let argv = Array.of_list (normalize_args (Array.to_list Sys.argv)) in
  let code =
    Cmd.eval ~catch:false ~argv (Cmd.group (Cmd.info "kiln") [ prepare ])
  in
  exit (if code = 0 then 0 else 1)

let () =
  try main () with
  | Sys_error message -> fail message
  | Unix.Unix_error (error, operation, path) ->
      fail (operation ^ " " ^ path ^ ": " ^ Unix.error_message error)
