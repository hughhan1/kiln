module Build_plan = Kiln_planning.Build_plan
module Json = Kiln_planning.Json
module Package = Kiln_planning.Package
module Platforms = Kiln_planning.Platforms
module Provider = Kiln_planning.Provider
module Registry = Kiln_planning.Registry
module Request = Kiln_planning.Request
module Source = Kiln_planning.Source
module Support = Kiln_planning.Support
module Error = Kiln_core.Error
module Semver = Kiln_core.Semver
open Result.Syntax

let parse_version value =
  Result.map_error
    (fun message -> Error.Metadata ("version", message))
    (Semver.version value)

let parse_range value =
  Result.map_error
    (fun message -> Error.Metadata ("version range " ^ value, message))
    (Semver.range value)

type day = Day of int * int * int

let day field text =
  let invalid () = Json.metadata field "expected an ISO calendar date" in
  match String.split_on_char '-' text with
  | [ year; month; day ]
    when String.length year = 4
         && String.length month = 2
         && String.length day = 2
         && List.for_all
              (String.for_all (function '0' .. '9' -> true | _ -> false))
              [ year; month; day ] ->
      let y, m, d =
        (int_of_string year, int_of_string month, int_of_string day)
      in
      let leap = y mod 4 = 0 && (y mod 100 <> 0 || y mod 400 = 0) in
      let days =
        match m with
        | 2 -> if leap then 29 else 28
        | 4 | 6 | 9 | 11 -> 30
        | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
        | _ -> 0
      in
      if y > 0 && d > 0 && d <= days then Ok (Day (y, m, d)) else invalid ()
  | [] | _ :: _ -> invalid ()

type manager = Npm | Pnpm

let name = function Npm -> "npm" | Pnpm -> "pnpm"

let manager = function
  | "npm" -> Ok Npm
  | "pnpm" -> Ok Pnpm
  | value -> Error (Error.Unsupported ("package manager " ^ value))

type node_request = Stable | Lts of string option | Range of Semver.range

let node_request text =
  match text with
  | "node" | "stable" -> Ok Stable
  | "lts/*" -> Ok (Lts None)
  | _ when String.starts_with ~prefix:"lts/" text ->
      Ok (Lts (Some (String.lowercase_ascii (String.drop_first 4 text))))
  | _ ->
      let+ r = parse_range text in
      Range r

type resolution = {
  key : string;
  node : Semver.version;
  pm : manager;
  pm_version : Semver.version;
  manager_install : Build_plan.argv option;
  builder : Build_plan.image;
  runtime : Build_plan.image;
}

let decode value =
  let* o = Json.object_ "resolution" value in
  let* key = Json.field "key" Json.string o in
  let* node = Json.field "nodeVersion" (Json.text parse_version) o in
  let* pm = Json.field "packageManager" (Json.text manager) o in
  let* pm_version =
    Json.field "packageManagerVersion" (Json.text parse_version) o
  in
  let* manager_install =
    Json.field "packageManagerInstall"
      (Json.optional (fun label value ->
           let* args = Json.list Json.string label value in
           Build_plan.argv args))
      o
  in
  let* builder = Json.field "builderImage" (Json.text Build_plan.image) o in
  let+ runtime = Json.field "runtimeImage" (Json.text Build_plan.image) o in
  { key; node; pm; pm_version; manager_install; builder; runtime }

let encode r =
  `Assoc
    [
      ("key", `String r.key);
      ("nodeVersion", `String (Semver.show r.node));
      ("packageManager", `String (name r.pm));
      ("packageManagerVersion", `String (Semver.show r.pm_version));
      ( "packageManagerInstall",
        match r.manager_install with
        | None -> `Null
        | Some command -> Json.string_array (Build_plan.arguments command) );
      ("builderImage", `String (Build_plan.image_ref r.builder));
      ("runtimeImage", `String (Build_plan.image_ref r.runtime));
    ]

type workspace = Packages_only | With_settings

let workspace bytes =
  let yaml result =
    Result.map_error
      (fun (`Msg message) -> Error.Metadata ("pnpm-workspace.yaml", message))
      result
  in
  let* value = yaml (Yaml.of_string bytes) in
  let* keys = yaml (Yaml.Util.keys value) in
  let* patterns = yaml (Yaml.Util.find "packages" value) in
  let* patterns =
    match patterns with
    | None | Some `Null -> Ok []
    | Some (`A values) ->
        Support.traverse (fun value -> yaml (Yaml.Util.to_string value)) values
    | Some (`Bool _ | `Float _ | `String _ | `O _) ->
        Json.metadata "packages" "expected array"
  in
  let+ () = Source.workspace patterns in
  if List.exists (fun key -> key <> "packages") keys then With_settings
  else Packages_only

type snapshot = {
  files : string list;
  packages : (string * Javascript.package) list;
  version_files : Package.requirement list;
  workspace : workspace option;
  source_links : bool;
}

type inputs = Whole_source | Manifests of string list

type application = {
  targets : Platforms.t;
  nodes : node_request list;
  node_declarations : Package.requirement list;
  manager_declarations : Package.requirement list;
  manager_ranges : Semver.range list;
  integrity : Javascript.integrity option;
  manager : manager;
  install : Build_plan.argv;
  install_commands : Build_plan.operation list -> Build_plan.operation list;
  inputs : inputs;
  build : Build_plan.operation list;
  command : Build_plan.argv option;
  fingerprint : string;
}

let selected_manager pkg (snapshot : snapshot) =
  match pkg.Javascript.managers with
  | [] ->
      Ok
        (if
           List.mem "pnpm-lock.yaml" snapshot.files
           || Option.is_some snapshot.workspace
         then Pnpm
         else Npm)
  | first :: rest ->
      if List.for_all (fun d -> d.Javascript.name = first.Javascript.name) rest
      then manager first.Javascript.name
      else Error (Error.Conflict "explicit package managers")

(* The source-relative path of a local dependency declared by the manifest
   [from], or [None] when it is absolute or names the source root. Loading has
   already rejected targets outside the source. *)
let local_input ~from reference =
  let rec normalize parts = function
    | [] -> Some (List.rev parts)
    | ("" | ".") :: rest -> normalize parts rest
    | ".." :: rest -> (
        match parts with [] -> None | _ :: parts -> normalize parts rest)
    | part :: rest -> normalize (part :: parts) rest
  in
  if not (Filename.is_relative reference) then None
  else
    match
      normalize []
        (String.split_on_char '/' (Filename.dirname from ^ "/" ^ reference))
    with
    | None | Some [] -> None
    | Some parts -> Some (String.concat "/" parts)

let install_inputs (snapshot : snapshot) =
  let requires_source (_, package) =
    package.Javascript.needs_source
    || List.exists
         (Javascript.has_script package)
         [
           "preinstall";
           "install";
           "postinstall";
           "prepare";
           "prepublish";
           "preprepare";
           "postprepare";
         ]
  in
  let full =
    snapshot.source_links
    || snapshot.workspace = Some With_settings
    || List.exists requires_source snapshot.packages
    || List.exists
         (fun path ->
           List.mem (Filename.basename path)
             [ "binding.gyp"; ".npmrc"; ".pnpmfile.cjs"; "pnpmfile.cjs" ]
           || String.starts_with ~prefix:"patches/" path)
         snapshot.files
  in
  (* Local packages are dependency inputs: they are copied whole, with the
     manifests, before installation. Workspace projects keep installing from
     the whole source when they reference local packages. *)
  let workspaces =
    Option.is_some snapshot.workspace
    || List.exists
         (fun (_, package) -> package.Javascript.workspaces <> [])
         snapshot.packages
  in
  let references =
    List.concat_map
      (fun (from, package) ->
        List.map (local_input ~from) package.Javascript.references)
      snapshot.packages
  in
  if full || List.mem None references || (workspaces && references <> []) then
    Whole_source
  else
    let referenced path =
      List.exists
        (function
          | Some target ->
              path = target || String.starts_with ~prefix:(target ^ "/") path
          | None -> false)
        references
    in
    Manifests
      (List.filter
         (fun path ->
           referenced path
           || List.mem (Filename.basename path)
                [
                  "package.json";
                  "package-lock.json";
                  "npm-shrinkwrap.json";
                  "pnpm-lock.yaml";
                  "pnpm-workspace.yaml";
                ])
         snapshot.files)

let interpret (request : Request.t) (snapshot : snapshot) =
  let packages = snapshot.packages in
  let* pkg =
    match List.assoc_opt "package.json" packages with
    | Some p -> Ok p
    | None -> Json.metadata "package.json" "missing"
  in
  let* () =
    if
      List.for_all (fun d -> d.Javascript.name = "node") pkg.Javascript.runtimes
    then Ok ()
    else Error (Error.Unsupported "only Node runtime is supported")
  in
  let node_declarations =
    Request.defaults request "node"
      (snapshot.version_files
      @ Package.optional "package.json engines.node"
          (List.assoc_opt "node" pkg.Javascript.engines)
      @ Javascript.requests ~path:"package.json" pkg.Javascript.runtimes)
  in
  let nodes_text = Package.versions node_declarations in
  let* nodes = Support.traverse node_request nodes_text in
  let* pm = selected_manager pkg snapshot in
  let manager_declarations =
    Request.defaults request (name pm)
      (Javascript.requests ~path:"package.json" pkg.Javascript.managers)
  in
  let manager_text = Package.versions manager_declarations in
  let* manager_ranges = Support.traverse parse_range manager_text in
  let* build_argv = Build_plan.argv [ name pm; "run"; "build" ] in
  let* command =
    Javascript.startup pkg ~override:(Request.start request)
      ~script:[ name pm; "run"; "start" ]
      ~main:[ "node" ]
  in
  let inputs =
    if Request.replaces_install request then Whole_source
    else install_inputs snapshot
  in
  let fingerprint =
    Registry.sha256
      (Json.encode
         (`Assoc
            [
              ( "kiln",
                `String ("ocaml-0.0.1:" ^ Registry.sha256 Node_toolchains.json)
              );
              ( "platforms",
                Json.string_array
                  (Platforms.to_strings request.Request.platforms) );
              ("node", Json.string_array nodes_text);
              ("manager", `String (name pm));
              ("managerRanges", Json.string_array manager_text);
              ( "integrity",
                match pkg.Javascript.declared_integrity with
                | None -> `Null
                | Some h -> `String (Javascript.integrity_text h) );
            ]))
  in
  let+ install =
    Request.install_command request
      ([ name pm; "install" ]
      @
      match pm with
      | Pnpm when List.mem "pnpm-lock.yaml" snapshot.files ->
          [ "--frozen-lockfile"; "--prefer-offline" ]
      | Npm | Pnpm -> [])
  in
  {
    targets = request.Request.platforms;
    nodes;
    node_declarations;
    manager_declarations;
    manager_ranges;
    integrity = pkg.Javascript.declared_integrity;
    manager = pm;
    install;
    install_commands = Request.install_commands request;
    inputs;
    build =
      Request.build_command request
        (if Javascript.has_script pkg "build" then Some build_argv else None);
    command;
    fingerprint;
  }

type candidate = {
  candidate_version : Semver.version;
  candidate_npm : Semver.version option;
  candidate_lts : string option;
}

type node_release = { version : Semver.version; npm : Semver.version }

let select_node app ~today ~releases ~schedule =
  let* today = day "today" today in
  let parse label value =
    let* o = Json.object_ label value in
    let* version = Json.field "version" (Json.text parse_version) o in
    let* npm = Json.field "npm" (Json.optional (Json.text parse_version)) o in
    let+ lts =
      match Json.member "lts" o with
      | `String name -> Ok (Some (String.lowercase_ascii name))
      | `Bool false -> Ok None
      | `Null | `Bool true | `Int _ | `Float _ | `List _ | `Assoc _ ->
          Json.metadata "Node LTS" "expected name or false"
    in
    { candidate_version = version; candidate_npm = npm; candidate_lts = lts }
  in
  let* values = Json.decode "Node releases" (Json.list parse) releases in
  let support label value =
    let* fields = Json.object_ label value in
    let* start = Json.field "start" (Json.text (day "support start")) fields in
    let+ finish = Json.field "end" (Json.text (day "support end")) fields in
    (start, finish)
  in
  let* support = Json.decode "Node schedule" (Json.assoc support) schedule in
  let eligible r =
    Semver.stable r.candidate_version
    && (match
          List.assoc_opt
            ("v" ^ string_of_int (Semver.major r.candidate_version))
            support
        with
      | None -> false
      | Some (start, finish) -> start <= today && today < finish)
    && (app.nodes <> [] || Option.is_some r.candidate_lts)
    && List.for_all
         (function
           | Stable -> true
           | Lts None -> Option.is_some r.candidate_lts
           | Lts (Some name) -> r.candidate_lts = Some name
           | Range range -> Semver.matches range r.candidate_version)
         app.nodes
  in
  let selected =
    Support.maximum
      ~compare:(fun a b ->
        Semver.compare a.candidate_version b.candidate_version)
      (List.filter eligible values)
  in
  match selected with
  | None ->
      Error
        (Error.No_version
           {
             tool = "Node";
             requirements =
               "no supported stable Node release satisfies the runtime \
                declarations; EOL lines are excluded";
           })
  | Some { candidate_npm = None; _ } ->
      Error
        (Error.Missing_metadata
           "selected Node release is missing its bundled npm version")
  | Some { candidate_version = version; candidate_npm = Some npm; _ } ->
      Ok { version; npm }

let select_manager app node bytes =
  let parse (version_text, value) =
    match Semver.version version_text with
    | Error _ -> Ok None
    | Ok version ->
        let* o = Json.object_ "package version" value in
        let* engines = Json.field "engines" (Json.default [] Json.object_) o in
        let* range = Json.field "node" (Json.default "*" Json.string) engines in
        let* deprecated =
          Json.field "deprecated" (Json.default "" Json.string) o
        in
        let* range = parse_range range in
        Ok
          (if
             List.for_all
               (fun range -> Semver.matches range version)
               app.manager_ranges
             && Semver.matches range node.version
             && (app.manager_ranges <> []
                || (Semver.stable version && deprecated = ""))
           then Some version
           else None)
  in
  let* fields = Json.decode "npm registry" Json.object_ bytes in
  let* versions = Json.field "versions" Json.object_ fields in
  let* versions = Support.traverse parse versions in
  Option.to_result
    ~none:
      (Error.No_version
         {
           tool = "package manager";
           requirements = "no eligible Node-compatible release";
         })
    (Support.maximum ~compare:Semver.compare (List.filter_map Fun.id versions))

let manager_tarball manager version =
  let manager = name manager in
  "https://registry.npmjs.org/" ^ manager ^ "/-/" ^ manager ^ "-"
  ^ Semver.show version ^ ".tgz"

let verify_integrity (Javascript.Integrity (kind, expected)) bytes =
  let module Hash = (val Digestif.module_of_hash' (kind :> Digestif.hash')) in
  let actual = Hash.(to_hex (digest_string bytes)) in
  if expected = actual then Ok () else Error Error.Integrity_mismatch

let resolve ~today ~fetch app =
  let* releases = fetch "https://nodejs.org/dist/index.json" None in
  let* schedule =
    fetch "https://raw.githubusercontent.com/nodejs/Release/main/schedule.json"
      None
  in
  let* node = select_node app ~today ~releases ~schedule in
  let pm = app.manager in
  let* pm_version =
    match (pm, app.manager_ranges, app.integrity) with
    | Npm, [], None -> Ok node.npm
    | (Npm | Pnpm), _, _ ->
        let* metadata = fetch ("https://registry.npmjs.org/" ^ name pm) None in
        select_manager app node metadata
  in
  let* () =
    match app.integrity with
    | None -> Ok ()
    | Some integrity ->
        let* bytes = fetch (manager_tarball pm pm_version) None in
        verify_integrity integrity bytes
  in
  let* packaged =
    let* entries =
      Json.decode "packaged Node toolchains" (Json.list Json.object_)
        Node_toolchains.json
    in
    let matches entry =
      app.integrity = None
      && Json.member "node" entry = `String (Semver.show node.version)
      && Json.member "manager" entry = `String (name pm)
      && Json.member "version" entry = `String (Semver.show pm_version)
    in
    match List.find_opt matches entries with
    | None -> Ok None
    | Some entry ->
        let* builder =
          Json.field "builder" (Json.text Build_plan.image) entry
        in
        let+ runtime =
          Json.field "runtime" (Json.text Build_plan.image) entry
        in
        Some (builder, runtime)
  in
  let* builder, runtime, manager_install =
    match packaged with
    | Some (builder, runtime) -> Ok (builder, runtime, None)
    | None ->
        let image suffix =
          Registry.image fetch ~platforms:app.targets ~repository:"library/node"
            (Semver.show node.version ^ suffix)
        in
        let* builder = image "-bookworm" in
        let* runtime = image "-bookworm-slim" in
        let+ install =
          match (pm, app.integrity) with
          | (Npm | Pnpm), Some (Javascript.Integrity (kind, hex)) ->
              let+ command =
                Build_plan.shell
                  (Printf.sprintf
                     "curl -fsSL %s -o /tmp/kiln-manager.tgz && echo '%s  \
                      /tmp/kiln-manager.tgz' | %ssum -c - && npm install \
                      --global /tmp/kiln-manager.tgz && rm \
                      /tmp/kiln-manager.tgz"
                     (manager_tarball pm pm_version)
                     hex
                     (Javascript.algorithm kind))
              in
              Some command
          | Npm, None when Semver.compare pm_version node.npm = 0 -> Ok None
          | (Npm | Pnpm), None ->
              let+ command =
                Build_plan.argv
                  [
                    "npm";
                    "install";
                    "--global";
                    name pm ^ "@" ^ Semver.show pm_version;
                  ]
              in
              Some command
        in
        (builder, runtime, install)
  in
  Ok
    {
      key = app.fingerprint;
      node = node.version;
      pm;
      pm_version;
      manager_install;
      builder;
      runtime;
    }

let id = "node"
let public_name = id
let exclude = [ "node_modules" ]

let plan app r =
  let open Build_plan in
  let pm = name r.pm in
  let* manager_env =
    match r.pm with
    | Npm -> Ok []
    | Pnpm ->
        let+ env =
          environment
            [
              ("npm_config_manage_package_manager_versions", "false");
              ("npm_config_package_manager_strict_version", "false");
              ("npm_config_pm_on_fail", "ignore");
            ]
        in
        [ Env env ]
  in
  let* ci_env = environment [ ("CI", "true") ] in
  let* runtime_env =
    environment
      [ ("NODE_ENV", "production"); ("HOME", "/tmp"); ("PORT", "8080") ]
  in
  let* toolchain =
    stage (Image r.builder)
      ([ Workdir "/app"; Env ci_env ]
      @ manager_env
      @ List.map (fun command -> Run command) (Option.to_list r.manager_install)
      )
  in
  let copy_source = Source.copy ~exclude [ "." ] "." in
  let install = Run app.install in
  let* build =
    stage (Stage toolchain)
      ((match app.inputs with
         | Whole_source -> copy_source :: app.install_commands [ install ]
         | Manifests paths ->
             app.install_commands (Source.manifests paths @ [ install ])
             @ [ copy_source ])
      @ app.build)
  in
  stage (Image r.runtime)
    ([ Workdir "/app"; Env runtime_env ]
    @ manager_env
    @ Option.fold ~none:[]
        ~some:(fun _ ->
          [
            copy toolchain
              [ "/usr/local/lib/node_modules/" ^ pm ^ "/" ]
              ("/usr/local/lib/node_modules/" ^ pm ^ "/");
            copy toolchain [ "/usr/local/bin/" ] "/usr/local/bin/";
          ])
        r.manager_install
    @ [
        copy ~owner:(10001, 10001)
          ~exclude:[ "*"; "!**/node_modules" ]
          build [ "/app/" ] "/app/";
        copy ~owner:(10001, 10001) ~exclude:[ "**/node_modules" ] build
          [ "/app/" ] "/app/";
        User (10001, 10001);
        Start app.command;
      ])

let detect source =
  if not (Source.has source "package.json") then Ok Provider.No_match
  else
    let+ pkg = Javascript.root source in
    if
      List.exists
        (fun d -> List.mem d.Javascript.name [ "npm"; "pnpm" ])
        pkg.Javascript.managers
      || List.exists
           (fun d -> d.Javascript.name = "node")
           pkg.Javascript.runtimes
    then Provider.Explicit
    else Provider.Fallback

let load request source =
  let* packages = Javascript.load source in
  let* versions =
    Support.traverse
      (fun path ->
        let+ version = Source.version source path in
        Package.optional path version)
      [ ".node-version"; ".nvmrc" ]
  in
  let* workspace =
    if Source.has source "pnpm-workspace.yaml" then
      let* bytes = source.Source.read "pnpm-workspace.yaml" in
      let+ workspace = workspace bytes in
      Some workspace
    else Ok None
  in
  interpret request
    {
      files = source.Source.files;
      packages;
      version_files = List.concat versions;
      workspace;
      source_links = source.Source.links;
    }

let startup app = app.command
let fingerprint app = app.fingerprint

let legacy app value =
  let+ r = decode value in
  if r.key = app.fingerprint then Some r else None

let packages app r =
  [
    Package.resolve "node" app.node_declarations (Semver.show r.node);
    Package.resolve
      ~default_source:
        (match r.pm with Npm -> "bundled with Node" | Pnpm -> "kiln default")
      (name r.pm) app.manager_declarations (Semver.show r.pm_version);
  ]

let metadata app =
  [ ("nodeRuntime", "node"); ("nodePackageManager", name app.manager) ]
