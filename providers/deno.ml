module Build_plan = Kiln_planning.Build_plan
module Json = Kiln_planning.Json
module Package = Kiln_planning.Package
module Jsonc = Kiln_planning.Jsonc
module Provider = Kiln_planning.Provider
module Request = Kiln_planning.Request
module Source = Kiln_planning.Source
module Support = Kiln_planning.Support
open Result.Syntax

let id = "deno"
let public_name = id
let exclude = [ "node_modules"; ".deno" ]
let configs = [ "deno.json"; "deno.jsonc" ]

let detect source =
  Ok
    (if
       List.exists (Source.has source)
         (".deno-version" :: "deno.lock" :: configs)
     then Provider.Explicit
     else Provider.No_match)

let task label value =
  match value with
  | `String command -> Ok (command <> "")
  | `Assoc fields ->
      let* command = Json.field "command" (Json.optional Json.string) fields in
      let+ dependencies =
        Json.field "dependencies"
          (Json.default [] (Json.list Json.string))
          fields
      in
      Option.fold ~none:false ~some:(( <> ) "") command || dependencies <> []
  | `Null | `Bool _ | `Float _ | `Int _ | `List _ ->
      Json.metadata label "expected task string or object"

let local_reference source ~from reference =
  if String.starts_with ~prefix:"file:" reference then
    Json.metadata from
      "file: URLs are unsupported; use a contained relative path"
  else if
    List.exists
      (fun prefix -> String.starts_with ~prefix reference)
      [ "./"; "../"; "/" ]
  then Source.check_reference source ~from reference
  else Ok ()

let config source path =
  let* bytes = source.Source.read path in
  let* fields = Jsonc.object_ path bytes in
  let* workspace =
    Json.field "workspace" (Json.default [] (Json.list Json.string)) fields
  in
  let* () = Source.workspace workspace in
  let* links =
    Json.field "links" (Json.default [] (Json.list Json.string)) fields
  in
  let* () = Source.workspace links in
  let* () =
    if List.exists (fun path -> String.contains path ':') (workspace @ links)
    then
      Json.metadata path
        "workspace and link entries must be contained relative paths"
    else Ok ()
  in
  let* imports =
    Json.field "imports" (Json.default [] (Json.assoc Json.string)) fields
  in
  let* scopes =
    Json.field "scopes"
      (Json.default [] (Json.assoc (Json.assoc Json.string)))
      fields
  in
  let* import_map = Json.field "importMap" (Json.optional Json.string) fields in
  let* () =
    match import_map with
    | None -> Ok ()
    | Some _ ->
        Json.metadata path
          "external importMap files are not supported; declare imports in \
           deno.json"
  in
  let* () =
    Support.iter
      (fun (_, reference) -> local_reference source ~from:path reference)
      (imports @ List.concat_map snd scopes)
  in
  let+ tasks = Json.field "tasks" (Json.default [] (Json.assoc task)) fields in
  (fields, tasks)

let prepare request source =
  let* path =
    match List.filter (Source.has source) configs with
    | [ path ] -> Ok path
    | [] -> Json.metadata "Deno" "requires deno.json or deno.jsonc"
    | _ :: _ -> Json.metadata "Deno" "keep only one of deno.json and deno.jsonc"
  in
  let* fields, tasks = config source path in
  let* () =
    Support.iter (config source)
      (List.filter
         (fun file -> file <> path && List.mem (Filename.basename file) configs)
         source.Source.files)
  in
  let* lock =
    match Json.member "lock" fields with
    | `Null | `Bool true -> Ok (Some "deno.lock")
    | `Bool false ->
        if Source.has source "deno.lock" then
          Json.metadata path "lock is disabled but deno.lock is present"
        else Ok None
    | `String path -> Ok (Some path)
    | `Assoc fields ->
        let+ path =
          Json.field "path" (Json.default "deno.lock" Json.string) fields
        in
        Some path
    | `Float _ | `Int _ | `List _ ->
        Json.metadata "Deno lock"
          "expected boolean, path, or lock configuration"
  in
  let rec relative path =
    if String.starts_with ~prefix:"./" path then
      relative (String.drop_first 2 path)
    else path
  in
  let* locked =
    match lock with
    | None -> Ok false
    | Some path ->
        let path = relative path in
        let* () = Source.workspace [ path ] in
        if not (Source.has source path) then Ok false
        else
          let* bytes = source.Source.read path in
          let+ _ = Json.decode path Json.object_ bytes in
          true
  in
  let* packages = Javascript.load source in
  let* declarations =
    Support.traverse
      (fun (path, pkg) -> Javascript.requirements ~path ~runtime:id pkg)
      packages
  in
  let* file_version = Source.version source ".deno-version" in
  let declarations =
    Request.defaults request id
      (Package.optional ".deno-version" file_version @ List.concat declarations)
  in
  let* requirements = Semver_image.requirements declarations in
  let* build_argv = Build_plan.argv [ "deno"; "task"; "build" ] in
  let* start =
    match Request.start request with
    | Some argv -> Ok (Some argv)
    | None when List.assoc_opt "start" tasks = Some true ->
        Result.map Option.some (Build_plan.argv [ "deno"; "task"; "start" ])
    | None -> Ok None
  in
  let* install =
    Request.install_command request
      ([ "deno"; "install" ] @ if locked then [ "--frozen" ] else [])
  in
  let* resolved =
    Semver_image.resolve ~tool:"Deno" ~pins:Pins.deno requirements
  in
  let open Build_plan in
  let* env =
    environment
      [
        ("DENO_DIR", "/deno-dir");
        ("DENO_NO_UPDATE_CHECK", "1");
        ("DENO_NO_PROMPT", "1");
      ]
  in
  let image = Semver_image.image resolved in
  let* mkdir = run [ "mkdir"; "-p"; "/deno-dir" ] in
  let* builder =
    stage (Image image)
      ([
         User (0, 0);
         Workdir "/app";
         Env env;
         mkdir;
         Source.copy ~exclude [ "." ] "/app/";
       ]
      @ Request.install_commands request [ Run install ]
      @ Request.build_command request
          (if List.assoc_opt "build" tasks = Some true then [ Run build_argv ]
           else []))
  in
  let+ plan =
    stage (Image image)
      [
        Workdir "/app";
        Env env;
        copy ~owner:(1000, 1000) builder [ "/app/" ] "/app/";
        copy ~owner:(1000, 1000) builder [ "/deno-dir/" ] "/deno-dir/";
        User (1000, 1000);
        Entrypoint None;
        Start start;
      ]
  in
  {
    Provider.plan;
    packages = [ Semver_image.package id requirements resolved ];
    metadata = [];
    resolution = Semver_image.encode resolved;
  }
