module Build_plan = Kiln_planning.Build_plan
module Json = Kiln_planning.Json
module Package = Kiln_planning.Package
module Platforms = Kiln_planning.Platforms
module Provider = Kiln_planning.Provider
module Registry = Kiln_planning.Registry
module Request = Kiln_planning.Request
module Source = Kiln_planning.Source
module Semver = Kiln_core.Semver
module Error = Kiln_core.Error
open Result.Syntax

let id = "go"
let public_name = id
let exclude = []

let detect source =
  Ok
    (if Source.has source "go.mod" then Provider.Explicit else Provider.No_match)

type application = {
  platforms : Platforms.t;
  requirements : Semver_image.requirements;
  minimum : Semver.version;
  install : Build_plan.argv;
  install_commands : Build_plan.operation list -> Build_plan.operation list;
  build : Build_plan.operation list;
  start : Build_plan.argv option;
}

type resolution = { compiler : Semver_image.t; runtime : Build_plan.image }

let words line =
  String.split_on_char ' '
    (String.map (fun c -> if c = '\t' then ' ' else c) line)
  |> List.filter (( <> ) "")

let directive name lines =
  let values =
    List.filter_map
      (fun line ->
        match words (String.trim line) with
        | key :: value :: _ when key = name -> Some value
        | _ -> None)
      lines
  in
  match values with
  | [] -> Ok None
  | [ value ] -> Ok (Some value)
  | _ -> Json.metadata "go.mod" ("duplicate " ^ name ^ " directive")

let parse_version value =
  let value =
    if List.length (String.split_on_char '.' value) = 2 then value ^ ".0"
    else value
  in
  Result.map_error
    (fun message -> Error.Metadata ("go.mod", message))
    (Semver.version value)

let load request source =
  let* () =
    if
      List.exists
        (fun path -> Filename.basename path = "go.work")
        source.Source.files
    then
      Json.metadata "Go application"
        "workspaces are unsupported; use one root module"
    else Ok ()
  in
  let* manifest = source.Source.read "go.mod" in
  let lines = String.split_on_char '\n' manifest in
  let* language = directive "go" lines in
  let* minimum =
    match language with
    | Some version -> parse_version version
    | None -> Json.metadata "go.mod" "a go version directive is required"
  in
  let* toolchain = directive "toolchain" lines in
  let* selected =
    match toolchain with
    | None | Some "default" -> Ok ("~" ^ Semver.show minimum)
    | Some value when String.starts_with ~prefix:"go" value ->
        let+ version = parse_version (String.drop_first 2 value) in
        Semver.show version
    | Some _ -> Json.metadata "go.mod" "unsupported Go toolchain"
  in
  let declarations =
    Request.defaults request id
      [ { Package.source = "go.mod"; version = selected } ]
  in
  let* requirements = Semver_image.requirements declarations in
  let* start =
    match Request.start request with
    | Some value -> Ok (Some value)
    | None -> Result.map Option.some (Build_plan.argv [ "/app/bin/server" ])
  in
  let* install =
    Request.install_command request [ "go"; "mod"; "download"; "all" ]
  in
  let* build =
    Build_plan.shell
      {|test "$(go list -mod=readonly -f '{{.Name}}' .)" = main || { echo 'Go applications require one executable root package' >&2; exit 1; }
test "$(CGO_ENABLED=1 go list -mod=readonly -f '{{len .CgoFiles}}' .)" = 0 || { echo 'CGo applications are unsupported' >&2; exit 1; }
go build -mod=readonly -trimpath -o /app/bin/server .|}
  in
  Ok
    {
      platforms = request.Request.platforms;
      requirements;
      minimum;
      install;
      install_commands = Request.install_commands request;
      build = Request.build_command request (Some build);
      start;
    }

let startup app = app.start

let fingerprint app =
  Semver_image.fingerprint app.requirements ^ Semver.show app.minimum

let resolve ~today:_ ~fetch app =
  let* compiler =
    Semver_image.resolve ~fetch ~repository:"library/golang"
      ~platforms:app.platforms
      ~accept:(fun version ->
        Semver.major version = 1 && Semver.compare version app.minimum >= 0)
      app.requirements
  in
  let+ runtime =
    Registry.image fetch ~platforms:app.platforms ~repository:"library/debian"
      "bookworm-slim"
  in
  { compiler; runtime }

let encode resolved =
  `Assoc
    [
      ("compiler", Semver_image.encode resolved.compiler);
      ("runtime", `String (Build_plan.image_ref resolved.runtime));
    ]

let decode value =
  let* fields = Json.object_ "Go resolution" value in
  let* compiler =
    Json.field "compiler" (fun _ value -> Semver_image.decode value) fields
  in
  let+ runtime = Json.field "runtime" (Json.text Build_plan.image) fields in
  { compiler; runtime }

let legacy _ _ = Error Error.Invalid_resolution

let packages app resolved =
  [ Semver_image.package id app.requirements resolved.compiler ]

let metadata _ = []

let plan app resolved =
  let open Build_plan in
  let* env =
    environment
      [
        ("CGO_ENABLED", "0");
        ("GOTOOLCHAIN", "local");
        ("GOFLAGS", "-mod=readonly");
        ("GOWORK", "off");
      ]
  in
  let* builder =
    stage
      (Image (Semver_image.image resolved.compiler))
      ([ Workdir "/app"; Env env; Source.copy ~exclude [ "." ] "/app/" ]
      @ app.install_commands [ Run app.install ]
      @ app.build)
  in
  stage (Image resolved.runtime)
    [
      Workdir "/app";
      copy ~owner:(1000, 1000) builder [ "/app/" ] "/app/";
      copy builder
        [ "/etc/ssl/certs/ca-certificates.crt" ]
        "/etc/ssl/certs/ca-certificates.crt";
      User (1000, 1000);
      Entrypoint None;
      Start app.start;
    ]
