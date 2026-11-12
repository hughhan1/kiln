open Result.Syntax

type startup = Native of Build_plan.argv | Shell of Build_plan.argv

type t = {
  source : string;
  environment : Environment.t;
  secrets : Environment.Names.t;
  install : Build_plan.argv option;
  install_steps : Commands.t option;
  start : startup option;
  error_missing_start : bool;
  provider : string option;
  build : Commands.t option;
  config_file : string;
  variables : Build_plan.environment option;
  development : bool;
}

let prefer value fallback =
  match value with Some _ -> value | None -> fallback

let optional_command parse label value =
  let* command = Json.default "" Json.string label value in
  match command with
  | "" -> Ok None
  | command ->
      let+ command = parse command in
      Some command

let shell_start_command script =
  let+ argv = Build_plan.argv [ "/bin/bash"; "-c"; script ] in
  Shell argv

let shell_start = optional_command shell_start_command

let start request =
  Option.map (function Native argv | Shell argv -> argv) request.start

let native_start label value =
  let* values = Json.list Json.string label value in
  Result.map_error
    (fun _ ->
      Error.Metadata
        (label, "expected nonempty executable-and-arguments without NUL"))
    (Build_plan.argv values)

let decode fields =
  let* environment = Json.field "environment" Environment.decode fields in
  let config name = Environment.config environment name in
  let configured name parse fallback =
    match config name with
    | None -> Ok fallback
    | Some value ->
        let+ parsed = parse value in
        Some parsed
  in
  let* install = configured "INSTALL_CMD" Build_plan.shell None in
  let* source = Json.field "source" Json.string fields in
  let* platforms =
    Json.field "platforms" (Json.optional (Json.list Json.string)) fields
  in
  let* () =
    let supported = [ "linux/amd64"; "linux/arm64"; "linux/arm64/v8" ] in
    match platforms with
    | None -> Ok ()
    | Some values
      when values <> [] && List.for_all (fun v -> List.mem v supported) values
      ->
        Ok ()
    | Some _ ->
        Json.metadata "platforms" "must contain linux/amd64 and/or linux/arm64"
  in
  let* error_missing_start =
    Json.field "errorMissingStartCommand"
      (Json.default false Json.boolean)
      fields
  in
  let* start = Json.field "start" (Json.optional native_start) fields in
  let* start_command = Json.field "startCommand" shell_start fields in
  let* start =
    match (start, start_command) with
    | Some _, Some _ ->
        Json.metadata "startCommand" "cannot combine with start argv"
    | Some argv, None -> Ok (Some (Native argv))
    | None, command -> configured "START_CMD" shell_start_command command
  in
  let* build =
    Json.field "buildCommand" (optional_command Commands.shell) fields
  in
  let* build = configured "BUILD_CMD" Commands.shell build in
  let* config_file =
    Json.field "configFilePath"
      (Json.default "railpack.json" Json.string)
      fields
  in
  let config_file = Option.value ~default:config_file (config "CONFIG_FILE") in
  let* development =
    Json.field "development" (Json.default false Json.boolean) fields
  in
  let+ provider = Json.field "provider" (Json.optional Json.string) fields in
  {
    source;
    environment;
    secrets = Environment.names environment;
    install;
    install_steps = None;
    start;
    error_missing_start;
    provider;
    build;
    config_file;
    variables = None;
    development;
  }

let build_command request default =
  Option.fold ~none:default
    ~some:(fun commands -> Commands.expand commands default)
    request.build

let install_commands request block =
  Option.fold ~none:block
    ~some:(fun commands -> Commands.expand commands block)
    request.install_steps

let replaces_install request =
  Option.is_some request.install
  || Option.fold ~none:false
       ~some:(fun commands -> not (Commands.inherits commands))
       request.install_steps

let install_command request default =
  match request.install with
  | Some argv -> Ok argv
  | None -> Build_plan.argv default

let version request name =
  Environment.config request.environment
    (String.uppercase_ascii name ^ "_VERSION")

let check_fields allowed label fields =
  match List.find_opt (fun (key, _) -> not (List.mem key allowed)) fields with
  | None -> Ok fields
  | Some (key, _) -> Json.metadata label ("unsupported field " ^ key)

let object_fields allowed label value =
  let* fields = Json.object_ label value in
  check_fields allowed label fields

let environment label value =
  let* variables = Json.default [] (Json.assoc Json.string) label value in
  match variables with
  | [] -> Ok None
  | _ :: _ ->
      let+ environment = Build_plan.environment variables in
      Some environment

let configure_fields request fields =
  let* provider = Json.field "provider" (Json.optional Json.string) fields in
  let* deploy =
    Json.field "deploy"
      (Json.default [] (object_fields [ "startCommand"; "variables" ]))
      fields
  in
  let* configured_start = Json.field "startCommand" shell_start deploy in
  let start =
    match request.start with
    | Some (Native _) -> request.start
    | None | Some (Shell _) -> prefer configured_start request.start
  in
  let* variables = Json.field "variables" environment deploy in
  let* steps =
    Json.field "steps"
      (Json.default [] (object_fields [ "install"; "build" ]))
      fields
  in
  let* install =
    Json.field "install" (Json.default [] (object_fields [ "commands" ])) steps
  in
  let* install_steps =
    Json.field "commands" (Json.optional (Commands.decode ~assets:[])) install
  in
  let* build =
    Json.field "build"
      (Json.default [] (object_fields [ "commands"; "assets" ]))
      steps
  in
  let* assets =
    Json.field "assets" (Json.default [] (Json.assoc Json.string)) build
  in
  let* build =
    Json.field "commands" (Json.optional (Commands.decode ~assets)) build
  in
  let+ secrets = Json.field "secrets" Environment.Names.decode fields in
  {
    request with
    provider = prefer provider request.provider;
    start;
    variables;
    secrets = Environment.Names.union request.secrets secrets;
    build = prefer build request.build;
    install =
      (match install_steps with Some _ -> None | None -> request.install);
    install_steps = prefer install_steps request.install_steps;
  }

let configure request source =
  let path = request.config_file in
  let* () = Source.workspace [ path ] in
  if Source.has source path then
    let* bytes = source.Source.read path in
    let* fields = Jsonc.object_ path bytes in
    let* fields =
      check_fields
        [ "$schema"; "provider"; "steps"; "deploy"; "secrets" ]
        path fields
    in
    configure_fields request fields
  else if path = "railpack.json" then Ok request
  else Json.metadata path "configuration file not found"

let defaults request name declarations =
  match version request name with
  | Some version ->
      [
        {
          Package.source =
            "RAILPACK_" ^ String.uppercase_ascii name ^ "_VERSION";
          version;
        };
      ]
  | None -> declarations
