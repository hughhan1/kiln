module Build_plan = Kiln_planning.Build_plan
module Json = Kiln_planning.Json
module Package = Kiln_planning.Package
module Provider = Kiln_planning.Provider
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
  node : Semver.version;
  pm : manager;
  pm_version : Semver.version;
  manager_install : Build_plan.argv option;
  builder : Build_plan.image;
  runtime : Build_plan.image;
}

let encode r =
  `Assoc
    [
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
  if full then Whole_source
  else
    Manifests
      (List.filter
         (fun path ->
           List.mem (Filename.basename path)
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
        (if Javascript.has_script pkg "build" then [ Build_plan.Run build_argv ]
         else []);
    command;
  }

(* The default is the newest LTS release; EOL lines never enter the pins. *)
let select_node app =
  Package.select ~tool:"Node" ~declarations:app.node_declarations
    ~version:(fun release -> release.Pins.node.Pins.release)
    ~parse:parse_version
    ~accept:(fun version release ->
      let lts = release.Pins.lts in
      (app.nodes <> [] || Option.is_some lts)
      && List.for_all
           (function
             | Stable -> true
             | Lts None -> Option.is_some lts
             | Lts (Some name) -> lts = Some name
             | Range range -> Semver.matches range version)
           app.nodes)
    Pins.node

(* npm installs a manager at build time, so an exact declaration needs no pin. *)
let select_manager app node =
  let allowed version =
    List.for_all (fun range -> Semver.matches range version) app.manager_ranges
  in
  let declared = Package.versions app.manager_declarations in
  match
    List.find_map (fun text -> Result.to_option (Semver.version text)) declared
  with
  | Some version when allowed version -> Ok version
  | Some _ | None ->
      let* tools =
        Support.traverse
          (fun tool ->
            let+ engines = parse_range tool.Pins.engines in
            (tool.Pins.tool, engines))
          (match app.manager with Npm -> Pins.npm | Pnpm -> Pins.pnpm)
      in
      let+ version, _ =
        Package.select ~tool:(name app.manager)
          ~declarations:
            (app.manager_declarations
            @ [ Package.require "Node" ("Node " ^ Semver.show node) ])
          ~version:fst ~parse:parse_version
          ~accept:(fun version (_, engines) ->
            allowed version && Semver.matches engines node)
          tools
      in
      version

let manager_tarball manager version =
  let manager = name manager in
  "https://registry.npmjs.org/" ^ manager ^ "/-/" ^ manager ^ "-"
  ^ Semver.show version ^ ".tgz"

let resolve app =
  let* node, release = select_node app in
  let* npm = parse_version release.Pins.npm in
  let pm = app.manager in
  let* pm_version =
    match (pm, app.manager_ranges, app.integrity) with
    | Npm, [], None -> Ok npm
    | (Npm | Pnpm), _, _ -> select_manager app node
  in
  let* builder = Build_plan.image release.Pins.node.Pins.builder in
  let* runtime = Build_plan.image release.Pins.node.Pins.runtime in
  let+ manager_install =
    match (pm, app.integrity) with
    | (Npm | Pnpm), Some (Javascript.Integrity (kind, hex)) ->
        let+ command =
          Build_plan.shell
            (Printf.sprintf
               "curl -fsSL %s -o /tmp/kiln-manager.tgz && echo '%s  \
                /tmp/kiln-manager.tgz' | %ssum -c - && npm install --global \
                /tmp/kiln-manager.tgz && rm /tmp/kiln-manager.tgz"
               (manager_tarball pm pm_version)
               hex
               (Javascript.algorithm kind))
        in
        Some command
    | Npm, None when Semver.compare pm_version npm = 0 -> Ok None
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
  { node; pm; pm_version; manager_install; builder; runtime }

let id = "node"
let public_name = id
let exclude = [ "node_modules" ]

(* npm honours --libc from 10.4. A replaced install command always copies the
   whole source, and pnpm has no equivalent flags. *)
let native app r =
  match (app.inputs, r.pm) with
  | Manifests _, Npm ->
      Semver.compare r.pm_version (Result.get_ok (parse_version "10.4.0")) >= 0
  | Manifests _, Pnpm | Whole_source, _ -> false

(* An install script may compile for the platform it runs on, so a package set
   with one installs under emulation. [cpu] sets $cpu to the platform's npm name. *)
let native_npm cpu =
  cpu
  ^ {|
if npm install --os=linux --cpu="$cpu" --libc=glibc --ignore-scripts \
  && ! grep -qs '"hasInstallScript": true' node_modules/.package-lock.json; then
  echo "kiln: installed Node dependencies natively for $cpu"
else
  echo "kiln: no native Node install for $cpu; installing under emulation"
  rm -rf node_modules
  touch .kiln-emulate
fi|}

let manager_env r =
  let open Build_plan in
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

let toolchain r from =
  let open Build_plan in
  let* manager_env = manager_env r in
  let* ci_env = environment [ ("CI", "true") ] in
  stage from
    ([ Workdir "/app"; Env ci_env ]
    @ manager_env
    @ List.map (fun command -> Run command) (Option.to_list r.manager_install))

(* Run each command unless the shell [test] succeeds. *)
let unless test =
  let open Build_plan in
  Support.traverse (function
    | Run command ->
        let+ command =
          argv
            ([ "/bin/sh"; "-c"; test ^ {| || exec "$@"|}; "kiln" ]
            @ arguments command)
        in
        Run command
    | ( Copy _ | Workdir _ | Env _ | User _ | Entrypoint _ | Start _
      | Prepend_path _ | Write _ ) as operation ->
        Ok operation)

let plan app r =
  let open Build_plan in
  let pm = name r.pm in
  let* manager_env = manager_env r in
  let* runtime_env =
    environment
      [ ("NODE_ENV", "production"); ("HOME", "/tmp"); ("PORT", "8080") ]
  in
  let* builder = toolchain r (Image r.builder) in
  let copy_source = Source.copy ~exclude [ "." ] "." in
  let install = Run app.install in
  let* steps =
    match app.inputs with
    | Whole_source -> Ok (copy_source :: app.install_commands [ install ])
    | Manifests paths when native app r ->
        let* base = toolchain r (Native r.builder) in
        let* native_install =
          shell
            (native_npm
               {|case "$TARGETARCH" in amd64) cpu=x64 ;; arm64) cpu=arm64 ;; esac|})
        in
        let* installed =
          stage (Stage base) (Source.manifests paths @ [ Run native_install ])
        in
        let+ emulated = unless "[ ! -e .kiln-emulate ]" [ install ] in
        app.install_commands (copy installed [ "/app/" ] "/app/" :: emulated)
        @ [ copy_source ]
    | Manifests paths ->
        Ok
          (app.install_commands (Source.manifests paths @ [ install ])
          @ [ copy_source ])
  in
  let* build = stage (Stage builder) (steps @ app.build) in
  stage (Image r.runtime)
    ([ Workdir "/app"; Env runtime_env ]
    @ manager_env
    @ Option.fold ~none:[]
        ~some:(fun _ ->
          [
            copy builder
              [ "/usr/local/lib/node_modules/" ^ pm ^ "/" ]
              ("/usr/local/lib/node_modules/" ^ pm ^ "/");
            copy builder [ "/usr/local/bin/" ] "/usr/local/bin/";
          ])
        r.manager_install
    @ [
        copy ~owner:(10001, 10001)
          ~exclude:[ "*"; "!**/node_modules" ]
          build [ "/app/" ] "/app/";
        copy ~owner:(10001, 10001)
          ~exclude:[ "**/node_modules"; ".kiln-emulate" ]
          build [ "/app/" ] "/app/";
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

let packages app r =
  [
    Package.resolve "node" app.node_declarations (Semver.show r.node);
    Package.resolve
      ~default_source:
        (match r.pm with Npm -> "bundled with Node" | Pnpm -> "kiln default")
      (name r.pm) app.manager_declarations (Semver.show r.pm_version);
  ]

let metadata app r =
  [
    ( "dependencyInstall",
      if native app r then "native, emulated if a package has an install script"
      else "emulated" );
    ("nodeRuntime", "node");
    ("nodePackageManager", name app.manager);
  ]

let frontend ~exclude source =
  if not (Source.has source "package.json") then Ok []
  else
    let* request = Request.decode [ ("source", `String "") ] in
    let* app = load request source in
    match app.build with
    | [] -> Ok []
    | _ :: _ ->
        let open Build_plan in
        let* r = resolve app in
        let commands = Run app.install :: app.build in
        let copy_source = Source.copy ~exclude [ "." ] "." in
        let* steps =
          match app.inputs with
          | Manifests paths when native app r ->
              let* base = toolchain r (Native r.builder) in
              let* check = shell (native_npm "cpu=$(node -p process.arch)") in
              let* build = unless "[ -e .kiln-emulate ]" app.build in
              let* built =
                stage (Stage base)
                  (Source.manifests paths @ (Run check :: copy_source :: build))
              in
              let+ fallback = unless "[ ! -e .kiln-emulate ]" commands in
              copy ~exclude:[ "**/node_modules" ] built [ "/app/" ] "/app/"
              :: fallback
          | Manifests _ | Whole_source -> Ok (copy_source :: commands)
        in
        let* target = toolchain r (Image r.builder) in
        let+ output = stage (Stage target) steps in
        [
          copy
            ~exclude:[ "**/node_modules"; ".kiln-emulate" ]
            output [ "/app/" ] "/app/";
        ]
