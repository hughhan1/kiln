module Build_plan = Kiln_planning.Build_plan
module Json = Kiln_planning.Json
module Provider = Kiln_planning.Provider
module Request = Kiln_planning.Request
module Source = Kiln_planning.Source
module Support = Kiln_planning.Support
module Semver = Kiln_planning.Semver
module Error = Kiln_planning.Error
open Result.Syntax

let id = "go"
let public_name = id
let includes = [ "node" ]

let words line =
  String.split_on_char ' '
    (String.map (fun c -> if c = '\t' then ' ' else c) line)
  |> List.filter (( <> ) "")

let header text =
  let rec close = function
    | [] -> []
    | line :: rest -> (
        match String.find_first ~sub:"*/" line with
        | None -> close rest
        | Some i -> String.sub line (i + 2) (String.length line - i - 2) :: rest
        )
  in
  let rec scan constraints = function
    | [] -> (constraints, None)
    | line :: rest -> (
        let line = String.trim line in
        let starts prefix = String.starts_with ~prefix line in
        let expression prefix =
          if starts prefix then
            Some (String.trim (String.drop_first (String.length prefix) line))
          else None
        in
        match List.find_map expression [ "//go:build"; "// +build" ] with
        | Some build -> scan (build :: constraints) rest
        | None -> (
            if line = "" || starts "//" then scan constraints rest
            else if starts "/*" then scan constraints (close (line :: rest))
            else
              match words line with
              | "package" :: name :: _ -> (constraints, Some name)
              | _ -> (constraints, None)))
  in
  scan [] (String.split_on_char '\n' text)

let detect source =
  if not (Source.has source "go.mod") then Ok Provider.No_match
  else
    let root path =
      Filename.extension path = ".go"
      && (not (String.contains path '/'))
      && not (Filename.check_suffix path "_test.go")
    in
    let+ headers =
      Support.traverse
        (fun path -> Result.map header (source.Source.read path))
        (List.filter root source.Source.files)
    in
    let main (constraints, name) =
      name = Some "main" && not (List.mem "ignore" constraints)
    in
    let library (constraints, name) =
      constraints = [] && Option.is_some name && name <> Some "main"
    in
    if List.exists main headers && not (List.exists library headers) then
      Provider.Explicit
    else Provider.No_match

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

let develop = Node.develop

let prepare request source =
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
  let* requirements = Semver_image.requirements [ selected ] in
  let* start =
    match Request.start request with
    | Some value -> Ok (Some value)
    | None -> Result.map Option.some (Build_plan.argv [ "/app/bin/server" ])
  in
  let* install = Build_plan.argv [ "go"; "mod"; "download"; "all" ] in
  let* frontend =
    Node.frontend ~exclude:[ "*.go"; "go.mod"; "go.sum" ] source
  in
  let* build =
    Build_plan.shell
      {|test "$(go list -mod=readonly -f '{{.Name}}' .)" = main || { echo 'Go applications require one executable root package' >&2; exit 1; }
test "$(CGO_ENABLED=1 go list -mod=readonly -f '{{len .CgoFiles}}' .)" = 0 || { echo 'CGo applications are unsupported' >&2; exit 1; }
go build -mod=readonly -trimpath -o /app/bin/server .|}
  in
  let* compiler =
    Semver_image.resolve ~tool:"Go"
      ~accept:(fun version -> Semver.compare version minimum >= 0)
      ~pins:Pins.golang requirements
  in
  let open Build_plan in
  let* runtime = image Pins.debian in
  let* builder =
    stage (Image compiler)
      ([
         Workdir "/app";
         Env
           [
             ("CGO_ENABLED", "0");
             ("GOTOOLCHAIN", "local");
             ("GOFLAGS", "-mod=readonly");
             ("GOWORK", "off");
           ];
         Source.copy [ "." ] "/app/";
       ]
      @ [ Run install ]
      @ if Request.builds request then frontend @ [ Run build ] else [])
  in
  stage (Image runtime)
    [
      Workdir "/app";
      copy ~owner:(1000, 1000) builder [ "/app/" ] "/app/";
      copy builder
        [ "/etc/ssl/certs/ca-certificates.crt" ]
        "/etc/ssl/certs/ca-certificates.crt";
      User (1000, 1000);
      Entrypoint;
      Start start;
    ]
