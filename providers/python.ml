module Build_plan = Kiln_planning.Build_plan
module Json = Kiln_planning.Json
module Package = Kiln_planning.Package
module Provider = Kiln_planning.Provider
module Request = Kiln_planning.Request
module Source = Kiln_planning.Source
module Support = Kiln_planning.Support
open Result.Syntax
module Environment = Kiln_planning.Environment
module Version = Python_version
module Toml = Python_toml

let id = "python"
let public_name = id
let uv_tool_version = "0.13.1"

let exclude =
  [
    "node_modules";
    ".venv";
    "venv";
    "__pycache__";
    ".pytest_cache";
    ".mypy_cache";
    ".ruff_cache";
  ]

let inputs =
  [
    "requirements.txt";
    "pyproject.toml";
    "uv.lock";
    "poetry.lock";
    "pdm.lock";
    ".python-version";
    "main.py";
  ]

let detect source =
  Ok
    (if List.exists (Source.has source) (List.filter (( <> ) "main.py") inputs)
     then Provider.Explicit
     else if Source.has source "main.py" then Provider.Fallback
     else Provider.No_match)

type uv = { version : Version.version; image : Build_plan.image }

type install =
  | Pip_install of {
      requirements : bool;
      project : bool;
      dependencies : string list;
    }
  | Uv_install of uv
  | Manager_install of {
      manager : manager;
      version : Version.version;
      lock : bool;
      install_root : bool;
    }

and manager = Poetry | Pdm

let manager_name = function Poetry -> "poetry" | Pdm -> "pdm"

type inputs = Whole_source | Manifests of string list

type application = {
  requirements : Version.requirement list;
  python_declarations : Package.requirement list;
  uv_declarations : Package.requirement list;
  manager_declarations : Package.requirement list;
  install : install;
  inputs : inputs;
  native : string list option;
  installable : bool;
  start : Build_plan.argv option;
  build : Build_plan.operation list;
  install_commands : Build_plan.operation list -> Build_plan.operation list;
  install_override : Build_plan.argv option;
}

type resolution = {
  python : Version.version;
  builder : Build_plan.image;
  runtime : Build_plan.image;
  installer : install;
}

let pip_arguments ~requirements ~dependencies =
  (if requirements then [ "-r"; "requirements.txt" ] else []) @ dependencies

let project source path =
  let* document = Toml.read source path in
  let* local = Python_sources.project source path document in
  let+ required = Toml.string document [ "project"; "requires-python" ] in
  (path, (document, required, local))

let root_manager document =
  let* poetry = Toml.field document [ "tool"; "poetry" ] Otoml.get_value in
  let* pdm = Toml.field document [ "tool"; "pdm" ] Otoml.get_value in
  let* backend = Toml.string document [ "build-system"; "build-backend" ] in
  let poetry =
    Option.is_some poetry || backend = Some "poetry.core.masonry.api"
  in
  let pdm = Option.is_some pdm || backend = Some "pdm.backend" in
  match (poetry, pdm) with
  | true, true ->
      Json.metadata "pyproject.toml"
        "ambiguous Python package manager declaration"
  | true, false -> Ok (Some Poetry)
  | false, true -> Ok (Some Pdm)
  | false, false -> Ok None

let manager_selection source root =
  let* declared = root_manager root in
  let locks =
    List.filter_map
      (fun (path, manager) ->
        if Source.has source path then Some (path, manager) else None)
      [
        ("uv.lock", None); ("poetry.lock", Some Poetry); ("pdm.lock", Some Pdm);
      ]
  in
  match locks with
  | _ :: _ :: _ ->
      Json.metadata "Python"
        "multiple lockfiles found; keep only the lockfile for the application \
         package manager"
  | [ (path, manager) ] ->
      let* () =
        match (manager, declared) with
        | None, Some _ ->
            Json.metadata path
              "uv.lock conflicts with the root package manager declaration"
        | Some lock_manager, Some declared_manager
          when lock_manager <> declared_manager ->
            Json.metadata path
              "lockfile conflicts with the root package manager declaration"
        | _ -> Ok ()
      in
      Ok (manager, true)
  | [] -> Ok (declared, false)

let manager_declarations request manager =
  let name = manager_name manager in
  let declarations = Request.defaults request name [] in
  let fallback = match manager with Poetry -> "2.5.1" | Pdm -> "2.29.5" in
  let* version =
    match Package.versions declarations with
    | [] -> Version.version fallback
    | [ version ] -> Version.version version
    | _ :: _ :: _ -> Json.metadata name "expected one exact version override"
  in
  Ok (declarations, version)

let standard_project root =
  match Toml.field root [ "project" ] Otoml.get_value with
  | Ok (Some _) -> true
  | Ok None | Error _ -> false

let validate_manager_metadata manager root =
  if not (standard_project root) then
    Json.metadata "pyproject.toml"
      "Poetry and PDM require modern [project] metadata; migrate dependencies \
       and Python requirements from [tool.poetry] into [project]"
  else
    match manager with
    | Pdm -> Ok ()
    | Poetry ->
        let* legacy_dependencies =
          Toml.field root [ "tool"; "poetry"; "dependencies" ] Otoml.get_value
        in
        let* modern_dependencies =
          Toml.field root [ "project"; "dependencies" ] Otoml.get_value
        in
        let* legacy_python =
          Toml.field root
            [ "tool"; "poetry"; "dependencies"; "python" ]
            Otoml.get_value
        in
        let* modern_python =
          Toml.string root [ "project"; "requires-python" ]
        in
        if Option.is_some legacy_python && Option.is_none modern_python then
          Json.metadata "pyproject.toml"
            "legacy Poetry Python constraints are unsupported; migrate them to \
             [project].requires-python"
        else if
          Option.is_some legacy_dependencies
          && Option.is_none modern_dependencies
        then
          Json.metadata "pyproject.toml"
            "legacy Poetry dependencies are unsupported; migrate them to \
             [project].dependencies"
        else Ok ()

let lock_references source manager path document =
  let path_fields = [ "editable"; "directory"; "virtual"; "path" ] in
  let rec collect key value =
    match value with
    | Otoml.TomlTable fields | Otoml.TomlInlineTable fields ->
        List.concat_map (fun (name, value) -> collect name value) fields
    | Otoml.TomlTableArray values | Otoml.TomlArray values ->
        List.concat_map (collect key) values
    | Otoml.TomlString text when List.mem key path_fields -> [ text ]
    | Otoml.TomlString text
      when key = "url"
           && (String.starts_with ~prefix:"file:" text
              || String.contains text '/' || String.contains text '$') ->
        [ text ]
    | Otoml.TomlString _ | Otoml.TomlInteger _ | Otoml.TomlFloat _
    | Otoml.TomlBoolean _ | Otoml.TomlOffsetDateTime _
    | Otoml.TomlLocalDateTime _ | Otoml.TomlLocalDate _ | Otoml.TomlLocalTime _
      ->
        []
  in
  let references = collect "" document in
  let+ local =
    Support.traverse
      (Python_sources.reference ~project_root:(manager = Pdm) source ~from:path)
      references
  in
  List.exists Fun.id local

let lock_requirements source lock =
  let* format =
    Toml.field lock [ "version" ] (Otoml.get_integer ~strict:true)
  in
  let* () =
    if format = Some 1 then Ok ()
    else Json.metadata "uv.lock" "unsupported lock format; regenerate with uv"
  in
  let* packages = Toml.array lock [ "package" ] Otoml.get_value in
  let check_package package =
    let* paths =
      Support.traverse
        (fun name -> Toml.string package [ "source"; name ])
        [ "editable"; "directory"; "virtual" ]
    in
    let paths = List.filter_map Fun.id paths in
    let* () =
      Support.iter (Source.check_reference source ~from:"uv.lock") paths
    in
    let+ file = Toml.field package [ "source"; "path" ] Otoml.get_value in
    (* The root project is installed separately, after the source copy. *)
    Option.is_some file || List.exists (( <> ) ".") paths
  in
  let* local = Support.traverse check_package packages in
  let+ requires = Toml.string lock [ "requires-python" ] in
  (requires, List.exists Fun.id local)

let uv_declarations request source root =
  let* uv_requirement = Toml.string root [ "tool"; "uv"; "required-version" ] in
  let* uv_config = Toml.read_optional source "uv.toml" in
  let* uv_config_requirement = Toml.string uv_config [ "required-version" ] in
  let declarations =
    Package.optional "pyproject.toml tool.uv.required-version" uv_requirement
    @ Package.optional "uv.toml required-version" uv_config_requirement
  in
  let parse, declarations =
    match (Request.version request "uv", declarations) with
    | None, _ :: _ -> (Version.specifier, declarations)
    | Some _, _ | None, [] ->
        ( (fun version -> Version.specifier ("==" ^ version)),
          Request.defaults request "uv" [] )
  in
  Ok (declarations, parse)

let install_inputs source install ~requirements ~project ~lock ~uv_local
    ~static_dependencies =
  let present = List.filter (Source.has source) in
  let manifests local paths =
    if local || source.Source.links then Whole_source
    else Manifests (List.sort_uniq String.compare paths)
  in
  match install with
  | Manager_install { install_root = true; _ } -> Whole_source
  | Pip_install { project = true; _ } ->
      manifests
        (project || requirements.Python_sources.local || not static_dependencies)
        requirements.Python_sources.included
  | Pip_install { project = false; _ } ->
      manifests requirements.Python_sources.local
        requirements.Python_sources.included
  | Uv_install _ ->
      manifests
        (project || lock || uv_local)
        ("pyproject.toml" :: "uv.lock"
        :: present [ "uv.toml"; ".python-version" ])
  | Manager_install { manager; install_root = false; _ } ->
      manifests (project || lock)
        ("pyproject.toml"
        :: present
             (match manager with
             | Poetry -> [ "poetry.lock"; "poetry.toml" ]
             | Pdm -> [ "pdm.lock"; "pdm.toml" ]))

let load request source =
  let* () =
    if List.exists (Source.has source) inputs then Ok ()
    else
      Json.metadata "Python"
        "requires a Python manifest, version file, or root main.py"
  in
  let* () =
    match List.find_opt (Source.has source) [ "Pipfile"; "Pipfile.lock" ] with
    | None -> Ok ()
    | Some file -> Json.metadata file "Pipenv projects are not supported"
  in
  let* projects =
    Support.traverse (project source)
      (List.filter
         (fun path -> Filename.basename path = "pyproject.toml")
         source.Source.files)
  in
  let root, root_local =
    Option.fold
      ~none:(Otoml.TomlTable [], false)
      ~some:(fun (document, _, local) -> (document, local))
      (List.assoc_opt "pyproject.toml" projects)
  in
  let* manager, has_lock = manager_selection source root in
  let* () =
    match manager with
    | None -> Ok ()
    | Some manager -> validate_manager_metadata manager root
  in
  let* pip_requirements = Python_sources.requirements source in
  let* lock =
    if Source.has source "uv.lock" then
      let+ document = Toml.read source "uv.lock" in
      Some document
    else Ok None
  in
  let* lock_requires, uv_lock_local =
    match lock with
    | None -> Ok (None, false)
    | Some lock -> lock_requirements source lock
  in
  let* manager_lock_local =
    match manager with
    | Some manager when has_lock ->
        let path =
          match manager with Poetry -> "poetry.lock" | Pdm -> "pdm.lock"
        in
        let* document = Toml.read source path in
        lock_references source manager path document
    | Some _ | None -> Ok false
  in
  let project_declarations =
    Package.optional "uv.lock requires-python" lock_requires
    @ List.concat_map
        (fun (path, (_, requirement, _)) ->
          Package.optional (path ^ " project.requires-python") requirement)
        projects
  in
  let* requested = Source.version source ".python-version" in
  let file_declarations, project_declarations =
    match Request.version request "python" with
    | Some _ -> (Request.defaults request "python" [], [])
    | None ->
        let file_declarations = Package.optional ".python-version" requested in
        let file_declarations =
          if project_declarations = [] then
            Request.defaults request "python" file_declarations
          else file_declarations
        in
        (file_declarations, project_declarations)
  in
  let requirements_text =
    List.sort_uniq String.compare (Package.versions project_declarations)
  in
  let* constraints = Support.traverse Version.specifier requirements_text in
  let python_declarations = file_declarations @ project_declarations in
  let* file_constraints =
    Support.traverse Version.version_file (Package.versions file_declarations)
  in
  let requirements = List.concat (file_constraints @ constraints) in
  let* name = Toml.string root [ "project"; "name" ] in
  let* build_system = Toml.field root [ "build-system" ] Otoml.get_table in
  let installable = Option.is_some name || Option.is_some build_system in
  let* uv_declarations, parse_uv = uv_declarations request source root in
  let uv_requirements =
    List.sort_uniq String.compare (Package.versions uv_declarations)
  in
  let* selected_manager =
    match manager with
    | None -> Ok None
    | Some manager ->
        let+ declarations, version = manager_declarations request manager in
        Some (manager, declarations, version)
  in
  let* dependencies = Toml.strings root [ "project"; "dependencies" ] in
  let* install =
    match (selected_manager, lock) with
    | Some (manager, _, version), _ ->
        let* install_root =
          match manager with
          | Poetry ->
              let+ package_mode =
                Toml.field root
                  [ "tool"; "poetry"; "package-mode" ]
                  (Otoml.get_boolean ~strict:true)
              in
              Option.value ~default:true package_mode
          | Pdm ->
              let+ distribution =
                Toml.field root
                  [ "tool"; "pdm"; "distribution" ]
                  (Otoml.get_boolean ~strict:true)
              in
              Option.is_some name && Option.value ~default:true distribution
        in
        Ok (Manager_install { manager; version; lock = has_lock; install_root })
    | None, None ->
        Ok
          (Pip_install
             {
               requirements = Source.has source "requirements.txt";
               project = installable;
               dependencies;
             })
    | None, Some _ ->
        if not (Source.has source "pyproject.toml") then
          Json.metadata "uv.lock" "requires pyproject.toml"
        else
          let* requirements = Support.traverse parse_uv uv_requirements in
          let requirements = List.concat requirements in
          let* version, pin =
            Package.select ~tool:"uv" ~declarations:uv_declarations
              ~version:(fun pin -> pin.Pins.version)
              ~parse:Version.version
              ~accept:(fun version _ -> Version.matches requirements version)
              Pins.uv
          in
          let+ image = Build_plan.image pin.Pins.image in
          Uv_install { version; image }
  in
  let* uv_package =
    Toml.field root [ "tool"; "uv"; "package" ] (Otoml.get_boolean ~strict:true)
  in
  let installable =
    match install with
    | Pip_install _ -> installable
    | Uv_install _ ->
        Option.value ~default:(Option.is_some build_system) uv_package
    | Manager_install { install_root; _ } -> install_root
  in
  let* workspace =
    Toml.field root [ "tool"; "uv"; "workspace" ] Otoml.get_value
  in
  (* uv builds the root project to read dynamic metadata. *)
  let* dynamic = Toml.field root [ "project"; "dynamic" ] Otoml.get_value in
  let* dynamic_fields = Toml.strings root [ "project"; "dynamic" ] in
  let inputs =
    if Request.replaces_install request then Whole_source
    else
      install_inputs source install ~requirements:pip_requirements
        ~project:root_local
        ~lock:(uv_lock_local || manager_lock_local)
        ~uv_local:(Option.is_some workspace || Option.is_some dynamic)
        ~static_dependencies:
          (Option.is_some name && not (List.mem "dependencies" dynamic_fields))
  in
  (* uv ignores pip's environment, so an index set through PIP_* stays with pip. *)
  let pip_index =
    List.exists
      (String.starts_with ~prefix:"PIP_")
      (Environment.Names.elements request.Request.secrets)
  in
  let native =
    match (install, inputs) with
    | Pip_install { requirements; dependencies; _ }, Manifests _
      when (requirements || dependencies <> []) && not pip_index ->
        Some (pip_arguments ~requirements ~dependencies)
    | ( (Pip_install _ | Uv_install _ | Manager_install _),
        (Whole_source | Manifests _) ) ->
        None
  in
  let* frontend = Node.frontend ~exclude source in
  let* scripts =
    Toml.table root [ "project"; "scripts" ] (Otoml.get_string ~strict:true)
  in
  let* start =
    match (Request.start request, scripts) with
    | Some argv, _ -> Ok (Some argv)
    | None, [ (name, _) ] when installable ->
        Result.map Option.some (Build_plan.argv [ name ])
    | None, ([] | _ :: _) ->
        if Source.has source "main.py" then
          Result.map Option.some (Build_plan.argv [ "python"; "main.py" ])
        else Ok None
  in
  Ok
    {
      requirements;
      python_declarations;
      uv_declarations;
      manager_declarations =
        Option.fold ~none:[]
          ~some:(fun (_, declarations, _) -> declarations)
          selected_manager;
      install;
      inputs;
      native;
      installable;
      start;
      install_override = request.Request.install;
      install_commands = Request.install_commands request;
      build = Request.build_command request frontend;
    }

let resolve app =
  let* python, toolchain =
    Package.select ~tool:"Python" ~declarations:app.python_declarations
      ~version:(fun pin -> pin.Pins.release)
      ~parse:Version.version
      ~accept:(fun version _ -> Version.matches app.requirements version)
      Pins.python
  in
  let* builder = Build_plan.image toolchain.Pins.builder in
  let+ runtime = Build_plan.image toolchain.Pins.runtime in
  { python; builder; runtime; installer = app.install }

let encode resolved =
  `Assoc
    [
      ("python", `String (Version.show resolved.python));
      ("builder", `String (Build_plan.image_ref resolved.builder));
      ("runtime", `String (Build_plan.image_ref resolved.runtime));
      ( "installer",
        `Assoc
          (match resolved.installer with
          | Pip_install _ -> [ ("name", `String "pip") ]
          | Uv_install uv ->
              [
                ("name", `String "uv");
                ("version", `String (Version.show uv.version));
                ("image", `String (Build_plan.image_ref uv.image));
              ]
          | Manager_install { manager; version; _ } ->
              [
                ("name", `String (manager_name manager));
                ("version", `String (Version.show version));
              ]) );
    ]

let manager_setup app resolved manager version checks install =
  let name = manager_name manager in
  match app.install_override with
  | Some _ -> Ok ([], [], [])
  | None ->
      let open Build_plan in
      let* venv = run [ "python"; "-m"; "venv"; "/opt/kiln-tools" ] in
      let* pip =
        run
          [
            "/opt/kiln-tools/bin/pip";
            "install";
            "--no-cache-dir";
            name ^ "==" ^ Version.show version;
          ]
      in
      let* tools = stage (Image resolved.builder) [ venv; pip ] in
      let defaults =
        List.map (fun args -> name :: args) (checks @ [ install ])
      in
      let env_values =
        match manager with
        | Poetry ->
            [
              ("POETRY_VIRTUALENVS_CREATE", "false");
              ("POETRY_VIRTUALENVS_IN_PROJECT", "false");
            ]
        | Pdm ->
            [
              ("PDM_PYTHON", "/opt/venv/bin/python");
              ("PDM_IGNORE_SAVED_PYTHON", "1");
            ]
      in
      Ok
        ( [ copy tools [ "/opt/kiln-tools/" ] "/opt/kiln-tools/" ],
          defaults,
          env_values )

let uv_sync options =
  [ "uv"; "sync"; "--locked"; "--no-dev" ]
  @ options
  @ [ "--python"; "/usr/local/bin/python"; "--no-managed-python" ]

(* The target's own architecture appears in every platform wheel uv selects;
   anything else in a tag came from building a source distribution here, so
   [reset] clears the way for an install under emulation. *)
let native_uv python ~what ~root ~reset arguments =
  Build_plan.argv
    ([
       "/bin/sh";
       "-c";
       {|case "$TARGETARCH" in amd64) arch=x86_64 ;; arm64) arch=aarch64 ;; esac
if /opt/kiln-tools/bin/uv pip install --python /opt/venv/bin/python --python-platform "$arch-manylinux_2_40" --python-version "$0" --compile-bytecode "$@" \
  && ! grep -hs '^Tag:' |}
       ^ root
       ^ {|/lib/*/site-packages/*.dist-info/WHEEL | grep -qv -e '-any$' -e "$arch"; then
  echo "kiln: installed Python |}
       ^ what
       ^ {| natively for $TARGETARCH"
else
  echo "kiln: no native Python install of |}
       ^ what ^ {| for $TARGETARCH; installing under emulation"
  rm -rf |}
       ^ root ^ "\n  " ^ reset ^ "\n  touch " ^ root ^ "/.kiln-emulate\nfi";
       Version.show python;
     ]
    @ arguments)

let emulated_install =
  {|[ ! -e /opt/venv/.kiln-emulate ] || { rm /opt/venv/.kiln-emulate && python -m pip install --no-cache-dir "$@"; }|}

let pip_project =
  "python -m pip install --no-cache-dir --no-deps --prefix=/opt/kiln-project ."

let overlay_project = "cp -R /opt/kiln-project/. /opt/venv/"

let compile_venv =
  Build_plan.shell "python -m compileall -qq -j 0 /opt/venv || true"

let plan app resolved =
  let open Build_plan in
  let* setup, defaults, manager_environment =
    match app.install with
    | Pip_install install ->
        let pip args =
          [ "python"; "-m"; "pip"; "install"; "--no-cache-dir" ] @ args
        in
        let requirements =
          if install.requirements then
            [ pip (pip_arguments ~requirements:true ~dependencies:[]) ]
          else []
        in
        let defaults =
          match app.inputs with
          | Whole_source ->
              requirements @ if install.project then [ pip [ "." ] ] else []
          | Manifests _ when install.dependencies = [] -> requirements
          | Manifests _ ->
              [
                pip
                  (pip_arguments ~requirements:install.requirements
                     ~dependencies:install.dependencies);
              ]
        in
        Ok ([], defaults, [])
    | Uv_install uv ->
        let* uv_stage = stage (Image uv.image) [] in
        let* env =
          environment
            [
              ("UV_PROJECT_ENVIRONMENT", "/opt/venv");
              ("UV_PYTHON_DOWNLOADS", "never");
              ("UV_LINK_MODE", "copy");
            ]
        in
        let defaults =
          match app.inputs with
          | Whole_source -> [ uv_sync [ "--no-editable" ] ]
          | Manifests _ -> [ uv_sync [ "--no-install-project" ] ]
        in
        Ok
          ( [ copy uv_stage [ "/uv" ] "/usr/local/bin/uv"; Env env ],
            defaults,
            [] )
    | Manager_install { manager = Poetry; version; lock; install_root } ->
        manager_setup app resolved Poetry version
          (if lock then [ [ "check"; "--lock" ] ] else [])
          ([ "install"; "--only"; "main"; "--no-interaction"; "--no-ansi" ]
          @ if install_root then [] else [ "--no-root" ])
    | Manager_install { manager = Pdm; version; lock; install_root } ->
        manager_setup app resolved Pdm version []
          ([ "install"; "--prod"; "--no-editable" ]
          @ (if lock then [ "--check" ] else [])
          @ if install_root then [] else [ "--no-self" ])
  in
  let* venv = run [ "python"; "-m"; "venv"; "/opt/venv" ] in
  let* commands =
    match app.install_override with
    | Some argv -> Ok [ Run argv ]
    | None -> Support.traverse run defaults
  in
  (* pip compiles what it installs; uv and the package managers do not. *)
  let* compile =
    match app.install with
    | Pip_install _ -> Ok []
    | Uv_install _ | Manager_install _ ->
        let+ compile = compile_venv in
        [ Run compile ]
  in
  (* Dependencies installed from manifests leave the project itself for after the source copy. *)
  let* finish =
    match (app.install, app.inputs) with
    | Uv_install _, Manifests _ when app.installable ->
        let+ sync = run (uv_sync [ "--no-editable" ]) in
        [ sync ]
    | Pip_install { project = true; _ }, Manifests _ ->
        let+ install = shell (pip_project ^ " && " ^ overlay_project) in
        [ Run install ]
    | ( (Pip_install _ | Uv_install _ | Manager_install _),
        (Whole_source | Manifests _) ) ->
        Ok []
  in
  let manager_path =
    match app.install with
    | Manager_install _ when Option.is_none app.install_override ->
        "/opt/kiln-tools/bin:"
    | Pip_install _ | Uv_install _ | Manager_install _ -> ""
  in
  let* env =
    environment
      ([
         ("VIRTUAL_ENV", "/opt/venv");
         ( "PATH",
           "/opt/venv/bin:" ^ manager_path
           ^ "/usr/local/bin:/usr/local/sbin:/usr/sbin:/usr/bin:/sbin:/bin" );
         ("PYTHONUNBUFFERED", "1");
         ("PYTHONDONTWRITEBYTECODE", "1");
       ]
      @ manager_environment)
  in
  let copy_source = Source.copy ~exclude [ "." ] "/app/" in
  (* The venv precedes the replaceable install block: the runtime stage copies it.
     Manifest-first builds install into a dependency stage that ends before the
     source copy, so a source edit leaves that stage's snapshot, and the venv
     copied from it, unchanged. *)
  let* dependencies, build, finish =
    match app.inputs with
    | Whole_source ->
        let+ builder =
          stage (Image resolved.builder)
            ([ Workdir "/app"; Env env; venv ]
            @ (copy_source :: app.install_commands (setup @ commands @ compile))
            @ app.build)
        in
        (builder, builder, finish)
    | Manifests paths ->
        let* install, finish =
          match app.native with
          | Some arguments ->
              let* tools =
                Support.traverse run
                  [
                    [ "python"; "-m"; "venv"; "/opt/kiln-tools" ];
                    [
                      "/opt/kiln-tools/bin/pip";
                      "install";
                      "--no-cache-dir";
                      "uv==" ^ uv_tool_version;
                    ];
                  ]
              in
              let* base =
                stage (Native resolved.builder)
                  ([ Workdir "/app"; Env env; venv ] @ tools)
              in
              let* native =
                native_uv resolved.python ~what:"dependencies" ~root:"/opt/venv"
                  ~reset:"/usr/local/bin/python -m venv /opt/venv" arguments
              in
              let* installed =
                stage (Stage base) (Source.manifests paths @ [ Run native ])
              in
              let* emulated =
                argv ([ "/bin/sh"; "-c"; emulated_install; "kiln" ] @ arguments)
              in
              let+ finish =
                match finish with
                | [] -> Ok []
                | _ :: _ ->
                    let* native =
                      native_uv resolved.python ~what:"project"
                        ~root:"/opt/kiln-project"
                        ~reset:"mkdir /opt/kiln-project"
                        [ "--no-deps"; "--prefix=/opt/kiln-project"; "." ]
                    in
                    let* built =
                      stage (Stage base) [ copy_source; Run native ]
                    in
                    let+ overlay =
                      shell
                        ("set -e; [ ! -e /opt/kiln-project/.kiln-emulate ] || \
                          { rm /opt/kiln-project/.kiln-emulate; " ^ pip_project
                       ^ "; }; " ^ overlay_project)
                    in
                    [
                      copy built [ "/opt/kiln-project/" ] "/opt/kiln-project/";
                      Run overlay;
                    ]
              in
              ( [ copy base [ "/opt/venv/" ] "/opt/venv/" ]
                @ app.install_commands
                    [
                      copy installed [ "/opt/venv/" ] "/opt/venv/";
                      copy installed [ "/app/" ] "/app/";
                      Run emulated;
                    ],
                finish )
          | None ->
              Ok
                ( venv
                  :: app.install_commands
                       (Source.manifests paths @ setup @ commands @ compile),
                  finish )
        in
        let* dependencies =
          stage (Image resolved.builder) ([ Workdir "/app"; Env env ] @ install)
        in
        let+ build =
          stage (Stage dependencies) ((copy_source :: finish) @ app.build)
        in
        (dependencies, build, finish)
  in
  (* Build commands do not change the venv of a manifest-first image. A project
     installs itself after the source copy: a pip project to a prefix, whose few
     files overlay the venv so a source edit leaves the venv layer unchanged, and
     a uv packaged project into the venv, which then comes from the build stage. *)
  let venv_source, project =
    match (app.install, finish) with
    | Pip_install _, _ :: _ ->
        ( dependencies,
          [
            copy ~owner:(1000, 1000) build [ "/opt/kiln-project/" ] "/opt/venv/";
          ] )
    | (Pip_install _ | Uv_install _ | Manager_install _), [] ->
        (dependencies, [])
    | (Uv_install _ | Manager_install _), _ :: _ -> (build, [])
  in
  let* compile_stdlib =
    run
      [
        "/usr/local/bin/python";
        "-m";
        "compileall";
        "-q";
        "-j";
        "0";
        "/usr/local/lib";
      ]
  in
  stage (Image resolved.runtime)
    ([
       Workdir "/app";
       Env env;
       compile_stdlib;
       copy ~owner:(1000, 1000) venv_source [ "/opt/venv/" ] "/opt/venv/";
     ]
    @ project
    @ [
        copy ~owner:(1000, 1000) build [ "/app/" ] "/app/";
        User (1000, 1000);
        Entrypoint None;
        Start app.start;
      ])

let packages app r =
  Package.resolve "python" app.python_declarations (Version.show r.python)
  ::
  (match r.installer with
  | Pip_install _ -> []
  | Uv_install uv ->
      [ Package.resolve "uv" app.uv_declarations (Version.show uv.version) ]
  | Manager_install { manager; version; _ } ->
      [
        Package.resolve (manager_name manager) app.manager_declarations
          (Version.show version);
      ])

let metadata app _ =
  [
    ( "dependencyInstall",
      if Option.is_some app.native then
        "native, emulated if a package has no target wheel"
      else "emulated" );
    ("pythonRuntime", "cpython");
    ( "pythonPackageManager",
      match app.install with
      | Pip_install _ -> "pip"
      | Uv_install _ -> "uv"
      | Manager_install { manager; _ } -> manager_name manager );
  ]
