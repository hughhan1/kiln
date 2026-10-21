module Build_plan = Kiln_planning.Build_plan
module Json = Kiln_planning.Json
module Package = Kiln_planning.Package
module Source = Kiln_planning.Source
module Support = Kiln_planning.Support
open Result.Syntax

type algorithm = [ `SHA224 | `SHA256 | `SHA384 | `SHA512 ]
type integrity = Integrity of algorithm * string

let algorithm = function
  | `SHA224 -> "sha224"
  | `SHA256 -> "sha256"
  | `SHA384 -> "sha384"
  | `SHA512 -> "sha512"

let integrity_text (Integrity (kind, hex)) = algorithm kind ^ "." ^ hex

let integrity value =
  let* kind, hex =
    match String.split_on_char '.' value with
    | [ "sha224"; hex ] -> Ok (`SHA224, hex)
    | [ "sha256"; hex ] -> Ok (`SHA256, hex)
    | [ "sha384"; hex ] -> Ok (`SHA384, hex)
    | [ "sha512"; hex ] -> Ok (`SHA512, hex)
    | [] | _ :: _ ->
        Json.metadata "packageManager integrity"
          "expected sha224/256/384/512 lowercase hex"
  in
  let module Hash = (val Digestif.module_of_hash' (kind :> Digestif.hash')) in
  match Hash.consistent_of_hex_opt hex with
  | Some hash when Hash.to_hex hash = hex -> Ok (Integrity (kind, hex))
  | None | Some _ ->
      Json.metadata "packageManager integrity" "invalid hex digest"

type declaration = { name : string; requested : string option; source : string }

let manager_names = [ "npm"; "pnpm"; "yarn"; "bun" ]

type package = {
  engines : (string * string) list;
  runtimes : declaration list;
  managers : declaration list;
  declared_integrity : integrity option;
  scripts : (string * string) list;
  main : string option;
  references : string list;
  needs_source : bool;
}

let declarations name dev =
  let parse label value =
    let* o = Json.object_ label value in
    let* name = Json.field "name" Json.string o in
    let+ requested = Json.field "version" (Json.optional Json.string) o in
    { name; requested; source = "devEngines." ^ label }
  in
  match Json.member name dev with
  | `Null -> Ok []
  | `List values -> Support.traverse (parse name) values
  | (`Assoc _ | `Bool _ | `Int _ | `Float _ | `String _) as value ->
      let+ one = parse name value in
      [ one ]

let package_manager spec =
  match String.split_first ~sep:"@" spec with
  | None -> Json.metadata "packageManager" "expected name@version"
  | Some (name, version) ->
      let* requested, hash =
        match String.split_last ~sep:"+" version with
        | Some (version, suffix) when String.starts_with ~prefix:"sha" suffix ->
            let+ hash = integrity suffix in
            (version, Some hash)
        | None | Some _ -> Ok (version, None)
      in
      if name = "" || requested = "" then
        Json.metadata "packageManager" "expected name@version"
      else
        Ok
          ( [ { name; requested = Some requested; source = "packageManager" } ],
            hash )

let local_reference value =
  let path prefixes value =
    if List.exists (fun prefix -> String.starts_with ~prefix value) prefixes
    then Some value
    else None
  in
  match String.split_first ~sep:":" value with
  | Some (("file" | "link"), reference) -> Some reference
  | Some ("workspace", reference) -> path [ "."; "/" ] reference
  | None | Some _ -> path [ "./"; "../"; "/" ] value

let package bytes =
  let* o = Json.decode "package.json" Json.object_ bytes in
  let* engines =
    Json.field "engines" (Json.default [] (Json.assoc Json.string)) o
  in
  let* dev = Json.field "devEngines" (Json.default [] Json.object_) o in
  let* runtimes = declarations "runtime" dev in
  let* dev_managers = declarations "packageManager" dev in
  let engine_managers =
    List.filter_map
      (fun name ->
        Option.map
          (fun version ->
            { name; requested = Some version; source = "engines." ^ name })
          (List.assoc_opt name engines))
      manager_names
  in
  let* explicit, integrity =
    Json.field "packageManager"
      (Json.default ([], None) (Json.text package_manager))
      o
  in
  let* scripts =
    Json.field "scripts" (Json.default [] (Json.assoc Json.string)) o
  in
  let* main = Json.field "main" (Json.optional Json.string) o in
  let* deps =
    Support.traverse
      (fun name -> Json.field name (Json.default [] (Json.assoc Json.string)) o)
      [ "dependencies"; "devDependencies"; "optionalDependencies" ]
  in
  let* workspaces =
    match Json.member "workspaces" o with
    | `Assoc fields -> Json.field "packages" (Json.list Json.string) fields
    | (`Null | `List _ | `Bool _ | `Int _ | `Float _ | `String _) as value ->
        Json.default [] (Json.list Json.string) "workspaces" value
  in
  let* () = Source.workspace workspaces in
  let injected label value =
    let* fields = Json.object_ label value in
    Json.field "injected" (Json.default false Json.boolean) fields
  in
  let* injected =
    Json.field "dependenciesMeta" (Json.default [] (Json.assoc injected)) o
  in
  let references =
    List.filter_map (fun (_, value) -> local_reference value) (List.concat deps)
  in
  Ok
    {
      engines;
      runtimes;
      managers = explicit @ dev_managers @ engine_managers;
      declared_integrity = integrity;
      scripts;
      main;
      references;
      needs_source =
        references <> [] || List.exists snd injected
        || Json.member "pnpm" o <> `Null;
    }

let has_script pkg name =
  Option.fold ~none:false ~some:(( <> ) "") (List.assoc_opt name pkg.scripts)

let startup pkg ~override ~script ~main =
  match (override, pkg.main) with
  | Some command, _ -> Ok (Some command)
  | None, _ when has_script pkg "start" ->
      Result.map Option.some (Build_plan.argv script)
  | None, Some entry when entry <> "" ->
      Result.map Option.some (Build_plan.argv (main @ [ entry ]))
  | None, (None | Some _) -> Ok None

let load source =
  let read_package path =
    let* bytes = source.Source.read path in
    let* pkg = package bytes in
    let+ () =
      Support.iter (Source.check_reference source ~from:path) pkg.references
    in
    (path, pkg)
  in
  Support.traverse read_package
    (List.filter
       (fun path -> Filename.basename path = "package.json")
       source.Source.files)

let root source =
  let* bytes = source.Source.read "package.json" in
  package bytes

let requests ~path declarations =
  List.concat_map
    (fun d -> Package.optional (path ^ " " ^ d.source) d.requested)
    declarations

let requirements ~path ~runtime pkg =
  let declarations = pkg.managers @ pkg.runtimes in
  let runtime_engines =
    List.filter (fun (name, _) -> not (List.mem name manager_names)) pkg.engines
  in
  match List.find_opt (fun d -> d.name <> runtime) declarations with
  | Some d -> Json.metadata runtime ("conflicting declaration for " ^ d.name)
  | None ->
      Ok
        (Package.optional
           (path ^ " engines." ^ runtime)
           (List.assoc_opt runtime runtime_engines)
        @ requests ~path declarations)
