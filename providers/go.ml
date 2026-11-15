module Build_plan = Kiln_planning.Build_plan
module Commands = Kiln_planning.Commands
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
let exclude = []

let package_name text start =
  let keyword = "package" in
  let length = String.length text in
  let after = start + String.length keyword in
  let identifier c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  if
    after < length
    && String.sub text start (String.length keyword) = keyword
    && String.contains " \t" text.[after]
  then
    let rec first i =
      if i < length && String.contains " \t" text.[i] then first (i + 1) else i
    in
    let begin_ = first after in
    let rec last i =
      if i < length && identifier text.[i] then last (i + 1) else i
    in
    match last begin_ - begin_ with
    | 0 -> None
    | size -> Some (String.sub text begin_ size)
  else None

let header text =
  let length = String.length text in
  let at i prefix =
    i + String.length prefix <= length
    && String.sub text i (String.length prefix) = prefix
  in
  let rec scan i constrained =
    if i >= length then (constrained, None)
    else if String.contains " \t\r\n" text.[i] then scan (i + 1) constrained
    else if at i "//" then
      let stop =
        Option.value (String.index_from_opt text i '\n') ~default:length
      in
      scan stop (constrained || at i "//go:build" || at i "// +build")
    else if at i "/*" then
      let rec close j =
        if j + 1 >= length then length
        else if text.[j] = '*' && text.[j + 1] = '/' then j + 2
        else close (j + 1)
      in
      scan (close (i + 2)) constrained
    else (constrained, package_name text i)
  in
  scan 0 false

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
    match
      List.filter_map
        (fun (constrained, name) -> if constrained then None else name)
        headers
    with
    | [] -> Provider.No_match
    | names ->
        if List.for_all (String.equal "main") names then Provider.Explicit
        else Provider.No_match

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
    Node.frontend ~exclude:([ "*.go"; "go.mod"; "go.sum" ] @ exclude) source
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
         Source.copy ~exclude [ "." ] "/app/";
       ]
      @ Commands.expand request.Request.install_steps [ Run install ]
      @ Commands.expand request.Request.build (frontend @ [ Run build ]))
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
