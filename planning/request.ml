open Result.Syntax

type t = {
  source : string;
  install_steps : Commands.t;
  start : Build_plan.argv option;
  provider : string option;
  build : Commands.t;
  config_file : string;
}

let start label value =
  let* values = Json.list Json.string label value in
  Result.map_error
    (fun _ ->
      Error.Metadata
        (label, "expected nonempty executable-and-arguments without NUL"))
    (Build_plan.argv values)

let decode fields =
  let* source = Json.field "source" Json.string fields in
  let* start = Json.field "start" (Json.optional start) fields in
  let+ config_file =
    Json.field "configFilePath"
      (Json.default "railpack.json" Json.string)
      fields
  in
  {
    source;
    install_steps = Commands.default;
    start;
    provider = None;
    build = Commands.default;
    config_file;
  }

let replaces_install request = not (Commands.inherits request.install_steps)

let check_fields allowed label fields =
  match List.find_opt (fun (key, _) -> not (List.mem key allowed)) fields with
  | None -> Ok fields
  | Some (key, _) -> Json.metadata label ("unsupported field " ^ key)

let object_fields allowed label value =
  let* fields = Json.object_ label value in
  check_fields allowed label fields

let commands name fields =
  let* step =
    Json.field name (Json.default [] (object_fields [ "commands" ])) fields
  in
  Json.field "commands" (Json.default Commands.default Commands.decode) step

let configure_fields request fields =
  let* provider = Json.field "provider" (Json.optional Json.string) fields in
  let* steps =
    Json.field "steps"
      (Json.default [] (object_fields [ "install"; "build" ]))
      fields
  in
  let* install_steps = commands "install" steps in
  let+ build = commands "build" steps in
  { request with provider; install_steps; build }

let configure request source =
  let path = request.config_file in
  let* () = Source.workspace [ path ] in
  if Source.has source path then
    let* bytes = source.Source.read path in
    let* fields = Jsonc.object_ path bytes in
    let* fields = check_fields [ "provider"; "steps" ] path fields in
    configure_fields request fields
  else if path = "railpack.json" then Ok request
  else Json.metadata path "configuration file not found"
