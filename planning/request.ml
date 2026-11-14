open Result.Syntax

type run = {
  command : Build_plan.argv;
  mounts : (Build_plan.input * string) list;
  overlay : Build_plan.input option;
  packages : string list;
}

type kind = Image of { start : Build_plan.argv option } | Run of run

type t = {
  source : string;
  install_steps : Commands.t;
  provider : string option;
  build : Commands.t;
  kind : kind;
}

let argv label value =
  let* values = Json.list Json.string label value in
  Result.map_error
    (fun _ ->
      Error.Metadata
        (label, "expected nonempty executable-and-arguments without NUL"))
    (Build_plan.argv values)

let check_fields allowed label fields =
  match List.find_opt (fun (key, _) -> not (List.mem key allowed)) fields with
  | None -> Ok fields
  | Some (key, _) -> Json.metadata label ("unsupported field " ^ key)

let object_fields allowed label value =
  let* fields = Json.object_ label value in
  check_fields allowed label fields

let context label value =
  let* name = Json.string label value in
  Build_plan.external_image name

let run label value =
  let* fields =
    object_fields [ "command"; "mounts"; "overlay"; "packages" ] label value
  in
  let* command = Json.field "command" argv fields in
  let* mounts =
    Json.field "mounts" (Json.default [] (Json.assoc Json.string)) fields
  in
  let* mounts =
    Support.traverse
      (fun (name, target) ->
        let+ input = Build_plan.external_image name in
        (input, target))
      mounts
  in
  let* overlay = Json.field "overlay" (Json.optional context) fields in
  let+ packages =
    Json.field "packages" (Json.default [] (Json.list Json.string)) fields
  in
  { command; mounts; overlay; packages }

let startup label value =
  let* fields = Json.default [] (object_fields [ "start" ]) label value in
  Json.field "start" (Json.optional argv) fields

let kinds = [ "image"; "development"; "run" ]

let development fields =
  match List.filter (fun (key, _) -> List.mem key kinds) fields with
  | [ ("development", `Assoc []) ] -> true
  | _ -> false

let kind fields =
  match List.filter (fun (key, _) -> List.mem key kinds) fields with
  | [] -> Ok (Image { start = None })
  | [ ("image", value) ] ->
      let+ start = startup "image" value in
      Image { start }
  | [ ("run", value) ] ->
      let+ run = run "run" value in
      Run run
  | _ ->
      Json.metadata "request"
        "expected one of image, run and an empty development object"

let decode fields =
  let* source = Json.field "source" Json.string fields in
  let+ kind = kind fields in
  {
    source;
    install_steps = Commands.default;
    provider = None;
    build =
      (match kind with Image _ -> Commands.default | Run _ -> Commands.none);
    kind;
  }

let start request =
  match request.kind with Image { start } -> start | Run _ -> None

let replaces_install request = not (Commands.inherits request.install_steps)

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
  {
    request with
    provider;
    install_steps;
    build =
      (match request.kind with Image _ -> build | Run _ -> Commands.none);
  }

let configure request source =
  let path = "railpack.json" in
  if Source.has source path then
    let* bytes = source.Source.read path in
    let* fields = Jsonc.object_ path bytes in
    let* fields = check_fields [ "provider"; "steps" ] path fields in
    configure_fields request fields
  else Ok request
