open Result.Syntax

let develop (module P : Provider.S) source start =
  let* { Provider.inputs; install; build; launcher } = P.develop source in
  let* start = Build_plan.argv (launcher @ Build_plan.arguments start) in
  let+ read =
    Support.traverse
      (fun path ->
        let+ contents = source.Source.read path in
        [
          path; "\000"; string_of_int (String.length contents); "\000"; contents;
        ])
      inputs
  in
  let digest =
    Digestif.SHA256.digesti_string (fun feed -> List.iter (List.iter feed) read)
  in
  let argv command = Json.string_array (Build_plan.arguments command) in
  let commands steps = `List (List.map argv steps) in
  Json.encode
    (`Assoc
       [
         ( "install",
           `Assoc
             [
               ("inputs", `String ("sha256:" ^ Digestif.SHA256.to_hex digest));
               ("commands", commands install);
             ] );
         ("prepare", commands build);
         ("start", argv start);
       ])

let output = "/tmp/kiln-run"

let run (module P : Provider.S) plan
    { Request.command; mounts; overlay; packages } =
  let* imported =
    match packages with
    | [] -> Ok []
    | _ :: _ when P.id <> "python" ->
        Error (Error.Unsupported ("packages for " ^ P.id))
    | _ :: _ ->
        let* install =
          Build_plan.run
            ([
               "python";
               "-m";
               "pip";
               "install";
               "--disable-pip-version-check";
               "--no-cache-dir";
               "--root-user-action=ignore";
               "--target";
               "/opt/kiln-packages";
             ]
            @ packages)
        in
        let+ installed = Build_plan.stage (Build_plan.base plan) [ install ] in
        [
          Build_plan.copy installed [ "/opt/kiln-packages/" ]
            "/opt/kiln-packages/";
          Build_plan.Env [ ("PYTHONPATH", "/opt/kiln-packages") ];
        ]
  in
  let* directory = Build_plan.argv [ "mkdir"; output ] in
  let* captured =
    Build_plan.argv
      ([
         "/bin/sh";
         "-c";
         Printf.sprintf {|"$@" >%s/stdout 2>%s/stderr; echo $? >%s/status|}
           output output output;
         "sh";
       ]
      @ Build_plan.arguments command)
  in
  let* running =
    Build_plan.stage (Build_plan.Stage plan)
      (Option.fold ~none:[]
         ~some:(fun input ->
           [
             Build_plan.Copy
               {
                 input;
                 paths = [ "." ];
                 exclude = [];
                 destination = "/app/";
                 owner = None;
               };
           ])
         overlay
      @ imported
      @ [
          Build_plan.Run directory;
          Build_plan.Isolated { command = captured; mounts };
        ])
  in
  Build_plan.stage Build_plan.Scratch
    [ Build_plan.copy running [ output ^ "/" ] "/" ]

let prepare (module P : Provider.S) (request : Request.t) source =
  let* plan = P.prepare request source in
  match request.Request.kind with
  | Request.Run command -> run (module P) plan command
  | Request.Image _ -> Ok plan

let failure error =
  Json.encode (`Assoc [ ("error", `String (Error.show error)) ])

let encode plan =
  Json.encode (`Assoc [ ("dockerfile", `String (Build_plan.dockerfile plan)) ])
