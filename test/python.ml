module Planner = Kiln_planning.Planner
module Build_plan = Kiln_planning.Build_plan
module Pins = Kiln_providers.Pins
module Version = Kiln_providers.Python_version
open Contract_support

let prepare root fields = Kiln_io.Driver.prepare (request root fields)
let python_pins = List.map (fun pin -> pin.Pins.release) Pins.python
let uv_pins = List.map (fun pin -> pin.Pins.version) Pins.uv
let oldest_python = List.hd python_pins
let newest_python = last python_pins

(* A pinned release's minor line, such as 3.13 for 3.13.7. *)
let minor release =
  match String.split_on_char '.' release with
  | major :: minor :: _ -> major ^ "." ^ minor
  | [] | [ _ ] -> failwith ("expected a release: " ^ release)

let selected response =
  Yojson.Basic.Util.(
    response.Planner.resolution |> member "data" |> member "python")

(* Builder steps as text: context copies and RUN argv. *)
let step = function
  | Build_plan.Copy { input = Build_plan.Context; paths = [ "." ]; _ } ->
      Some "source"
  | Build_plan.Copy { input = Build_plan.Context; paths; _ } ->
      Some ("manifest " ^ String.concat " " paths)
  | Build_plan.Run argv -> Some (String.concat " " (Build_plan.arguments argv))
  | Build_plan.Copy { input = Build_plan.Output _ | Build_plan.External _; _ }
  | Build_plan.Workdir _ | Build_plan.Env _ | Build_plan.User _
  | Build_plan.Entrypoint _ | Build_plan.Start _ | Build_plan.Prepend_path _
  | Build_plan.Write _ ->
      None

let own_steps stage = List.filter_map step (Build_plan.operations stage)

(* Steps of a stage preceded by those of the stages it is built from. *)
let rec inherited_steps stage =
  (match Build_plan.base stage with
    | Build_plan.Stage parent -> inherited_steps parent
    | Build_plan.Image _ | Build_plan.Native _ -> [])
  @ own_steps stage

let steps prepared =
  inherited_steps
    (List.find
       (fun stage -> List.mem "source" (own_steps stage))
       (List.rev (Build_plan.stages prepared.Planner.plan)))

(* Steps of every stage, each after the stages it depends on. *)
let all_steps prepared =
  List.concat_map own_steps (Build_plan.stages prepared.Planner.plan)

(* Whether the runtime copies the venv from a stage that copies the source. *)
let venv_follows_source prepared =
  List.exists
    (function
      | Build_plan.Copy { input; paths = [ "/opt/venv/" ]; _ } -> (
          match input with
          | Build_plan.Output stage -> List.mem "source" (own_steps stage)
          | Build_plan.Context | Build_plan.External _ -> false)
      | Build_plan.Copy _ | Build_plan.Run _ | Build_plan.Workdir _
      | Build_plan.Env _ | Build_plan.User _ | Build_plan.Entrypoint _
      | Build_plan.Start _ | Build_plan.Prepend_path _ | Build_plan.Write _ ->
          false)
    (Build_plan.operations prepared.Planner.plan)

(* Whether the first step starting with each prefix occurs in list order. *)
let ordered steps prefixes =
  let indices =
    List.map
      (fun prefix ->
        match List.find_index (String.starts_with ~prefix) steps with
        | Some index -> index
        | None -> failwith ("missing step: " ^ prefix))
      prefixes
  in
  indices = List.sort_uniq Int.compare indices

let () =
  List.iter
    (fun (constraint_, included, excluded) ->
      test ("Python requirement " ^ constraint_) (fun () ->
          let requirement = ok (Version.specifier constraint_) in
          let holds release =
            Version.matches requirement (ok (Version.version release))
          in
          check "matching releases" (List.for_all holds included);
          check "other releases" (not (List.exists holds excluded))))
    [
      ( ">=3.11,<3.14,!=3.12.*",
        [ "3.11.14"; "3.13.7" ],
        [ "3.10.9"; "3.12.12"; "3.14.0" ] );
      ("~=3.12.1", [ "3.12.1"; "3.12.12" ], [ "3.12.0"; "3.13.0" ]);
      ("~=3.12", [ "3.12.0"; "3.13.7" ], [ "3.11.0"; "4.0.0" ]);
      ("==3.12", [ "3.12.0" ], [ "3.12.1" ]);
      ("==3.12,>3.12", [], [ "3.12.0"; "3.12.1" ]);
      ("<=3.12.11,>3.11.14", [ "3.12.0"; "3.12.11" ], [ "3.11.14"; "3.12.12" ]);
    ];
  List.iter
    (fun release ->
      snapshot
        [ ("main.py", ""); (".python-version", minor release) ]
        ("Python " ^ minor release ^ " selects its pin")
        (fun root ->
          check "pinned release"
            (selected (ok (prepare root [])) = `String release)))
    python_pins;
  snapshot
    [ ("main.py", ""); (".python-version", "3.1.4") ]
    "an unsupported Python lists what is supported"
    (fun root ->
      check "supported releases"
        (Result.map selected (prepare root [])
        = Error
            (Kiln_core.Error.No_version
               {
                 tool = "Python";
                 requirements = "3.1.4";
                 supported = python_pins;
               })));
  snapshot
    [
      ("main.py", "print('hello')");
      (".python-version", minor oldest_python);
      ( "pyproject.toml",
        "[project]\nrequires-python = '>=3.11,!=" ^ newest_python ^ "'\n" );
    ]
    "Python intersects file and project constraints"
    (fun root ->
      let first = ok (prepare root []) in
      check "the file selects within the range"
        (selected first = `String oldest_python);
      let open Yojson.Basic.Util in
      let package =
        `Assoc (Planner.info first)
        |> member "resolvedPackages" |> member "python"
      in
      check "the plan reports the intersected request and its sources"
        (member "requestedVersion" package
         = `String (minor oldest_python ^ " && >=3.11,!=" ^ newest_python)
        && member "source" package
           = `String ".python-version, pyproject.toml project.requires-python");
      write root ".python-version" (minor newest_python);
      check "excluding the only release in a file's line leaves nothing"
        (Result.is_error (prepare root [])));
  snapshot
    [
      ("main.py", "");
      (".python-version", minor newest_python);
      ("pyproject.toml", "[project]\nrequires-python = '>=3.11'\n");
      ("uv.lock", "version=1\nrequires-python = '>=3.11'\n");
    ]
    "uv defaults and intersected declarations"
    (fun root ->
      let open Yojson.Basic.Util in
      let package prepared =
        `Assoc (Planner.info prepared)
        |> member "resolvedPackages" |> member "uv"
      in
      let default = package (ok (prepare root [])) in
      check "uv default has no declared version"
        (member "requestedVersion" default = `Null
        && member "source" default = `String "kiln default"
        && member "resolvedVersion" default = `String (last uv_pins));
      let oldest = List.hd uv_pins in
      write root "uv.toml" ("required-version = '>=" ^ oldest ^ "'\n");
      write root "pyproject.toml"
        ("[project]\n\
          requires-python = '>=3.11'\n\
          [tool.uv]\n\
          required-version = '<=" ^ oldest ^ "'\n");
      let uv = package (ok (prepare root [])) in
      check "explicit constraints select a pin and report their origins"
        (member "requestedVersion" uv
         = `String ("<=" ^ oldest ^ " && >=" ^ oldest)
        && member "resolvedVersion" uv = `String oldest
        && member "source" uv
           = `String
               "pyproject.toml tool.uv.required-version, uv.toml \
                required-version");
      write root "pyproject.toml"
        "[project]\n\
         requires-python = '>=3.11'\n\
         [tool.uv]\n\
         required-version = '==0.0.1'\n";
      check "an unsupported uv lists what is supported"
        (lists_supported (prepare root []) uv_pins));
  snapshot
    [
      ("main.py", "");
      (".python-version", "3.13");
      ( "pyproject.toml",
        "[project]\n\
         requires-python='>=3.13'\n\
         [tool.uv]\n\
         required-version='>=0.9'\n" );
      ("uv.lock", "version=1\nrequires-python='>=3.13'\n");
    ]
    "Python and uv environment versions replace declared constraints"
    (fun root ->
      let prepared =
        ok
          (prepare root
             [
               ( "environment",
                 `Assoc
                   [
                     ("RAILPACK_PYTHON_VERSION", `String oldest_python);
                     ("RAILPACK_UV_VERSION", `String (List.hd uv_pins));
                   ] );
             ])
      in
      let open Yojson.Basic.Util in
      let packages =
        `Assoc (Planner.info prepared) |> member "resolvedPackages"
      in
      List.iter
        (fun (name, version, setting) ->
          let package = member name packages in
          check "environment toolchain and provenance selected"
            (member "resolvedVersion" package = `String version
            && member "requestedVersion" package = `String version
            && member "source" package = `String setting))
        [
          ("python", oldest_python, "RAILPACK_PYTHON_VERSION");
          ("uv", List.hd uv_pins, "RAILPACK_UV_VERSION");
        ]);
  snapshot
    [
      ("requirements.txt", "idna==3.10\n-r requirements/base.txt\n");
      ("main.py", "");
    ]
    "remote requirements install before the source copy"
    (fun root ->
      Unix.mkdir (Filename.concat root "requirements") 0o700;
      write root "requirements/base.txt" "--hash=sha256:abc\nflask>=3\n";
      let prepared = ok (prepare root []) in
      check "the venv comes from the stage before the source copy"
        (not (venv_follows_source prepared));
      check "environment, manifests, native install, fallback, then source"
        (ordered (all_steps prepared)
           [
             "python -m venv";
             "manifest requirements.txt";
             "manifest requirements/base.txt";
             "/bin/sh -c case";
             "/bin/sh -c [ ! -e /opt/venv/.kiln-emulate ]";
             "source";
           ]);
      let dockerfile = Planner.dockerfile prepared in
      check "the fallback installs the same requirements"
        (contains dockerfile {|"kiln","-r","requirements.txt"|});
      check "manifests keep their own paths"
        (contains dockerfile {|COPY ["requirements.txt","./"]|}
        && contains dockerfile
             {|COPY ["requirements/base.txt","requirements/"]|}));
  let pyproject dependency =
    "[project]\nname='app'\ndynamic=['version']\ndependencies=['" ^ dependency
    ^ "']\n\
       [build-system]\n\
       requires=['setuptools']\n\
       build-backend='setuptools.build_meta'\n"
  in
  let project = pyproject "idna==3.10" in
  let install_mode prepared =
    Yojson.Basic.Util.(
      `Assoc (Planner.info prepared)
      |> member "metadata" |> member "dependencyInstall" |> to_string)
  in
  snapshot
    [ ("pyproject.toml", project); ("main.py", "") ]
    "pip project installs dependencies natively before the source"
    (fun root ->
      let prepared = ok (prepare root []) in
      let dockerfile = Planner.dockerfile prepared in
      check "the install stage runs on the build platform and targets the image"
        (contains dockerfile "FROM --platform=$BUILDPLATFORM"
        && contains dockerfile "ARG TARGETARCH"
        && contains dockerfile {|--python-platform \"$arch-manylinux_2_40\"|}
        && contains dockerfile {|"idna==3.10"]|});
      check "the plan names the install path"
        (String.starts_with ~prefix:"native" (install_mode prepared));
      check "the project installs alone after the source copy"
        (ordered (steps prepared)
           [
             "/bin/sh -c [ ! -e /opt/venv/.kiln-emulate ]";
             "source";
             "/bin/sh -c set -e; [ ! -e /opt/kiln-project/.kiln-emulate ]";
           ]);
      check "the project builds natively too"
        (contains dockerfile {|"--no-deps","--prefix=/opt/kiln-project","."|});
      check "a source edit leaves the venv layer alone"
        (not (venv_follows_source prepared));
      check "the runtime overlays the project and compiles the standard library"
        (contains dockerfile {|["/opt/kiln-project/","/opt/venv/"]|}
        && contains dockerfile {|"compileall","-q","-j","0","/usr/local/lib"|});
      write root "main.py" "print('edited')\n";
      write root "pyproject.toml" (project ^ "[tool.ruff]\nline-length=100\n");
      check "editing source or unrelated configuration keeps the plan"
        (Planner.dockerfile (ok (prepare root [])) = dockerfile);
      write root "pyproject.toml" (pyproject "idna==3.11");
      check "a dependency bump changes the install"
        (Planner.dockerfile (ok (prepare root [])) <> dockerfile));
  snapshot
    [ ("pyproject.toml", project); ("main.py", "") ]
    "pip's own environment keeps the install with pip"
    (fun root ->
      let prepared =
        ok
          (prepare root
             [
               ( "environment",
                 `Assoc [ ("PIP_INDEX_URL", `String "https://example.test") ] );
             ])
      in
      check "stays on the target platform"
        (not (contains (Planner.dockerfile prepared) "$BUILDPLATFORM"));
      check "the plan names the install path"
        (install_mode prepared = "emulated");
      check "dependencies still precede the source"
        (ordered (steps prepared)
           [
             "python -m pip install --no-cache-dir idna==3.10";
             "source";
             "/bin/sh -c python -m pip install --no-cache-dir --no-deps";
           ]));
  List.iter
    (fun (label, files, fields) ->
      snapshot files label (fun root ->
          Unix.mkdir (Filename.concat root "packages") 0o700;
          Unix.mkdir (Filename.concat root "packages/local") 0o700;
          write
            (Filename.concat root "packages/local")
            "pyproject.toml" "[project]\nname='local'\nversion='1.0.0'\n";
          let steps = steps (ok (prepare root fields)) in
          check "installation sees the whole source"
            (not (List.exists (String.starts_with ~prefix:"manifest") steps))))
    [
      ( "local requirement keeps the whole source",
        [ ("requirements.txt", "./packages/local\n"); ("main.py", "") ],
        [] );
      ( "dynamic dependencies keep the whole source",
        [
          ("requirements.txt", "idna==3.10\n");
          ( "pyproject.toml",
            "[project]\n\
             name='app'\n\
             version='1.0.0'\n\
             dynamic=['dependencies']\n\
             [build-system]\n\
             requires=['setuptools']\n\
             build-backend='setuptools.build_meta'\n" );
        ],
        [] );
      ( "custom install keeps the whole source",
        [ ("requirements.txt", "idna==3.10\n"); ("main.py", "") ],
        [
          ( "environment",
            `Assoc [ ("RAILPACK_INSTALL_CMD", `String "pip install idna") ] );
        ] );
      ( "uv workspace keeps the whole source",
        [
          ( "pyproject.toml",
            "[project]\n\
             name='app'\n\
             version='1.0.0'\n\
             requires-python='>=3.11'\n\
             [tool.uv.workspace]\n\
             members=['packages/local']\n" );
          ( "uv.lock",
            "version=1\n\
             [[package]]\n\
             name='local'\n\
             source={editable='packages/local'}\n" );
        ],
        [] );
      ( "uv dynamic metadata keeps the whole source",
        [
          ( "pyproject.toml",
            "[project]\n\
             name='app'\n\
             dynamic=['version']\n\
             requires-python='>=3.11'\n" );
          ("uv.lock", "version=1\n");
        ],
        [] );
    ];
  List.iter
    (fun (label, build_system, finish) ->
      snapshot
        [
          ( "pyproject.toml",
            "[project]\n\
             name='app'\n\
             version='1.0.0'\n\
             requires-python='>=3.11'\n\
             dependencies=['idna==3.10']\n" ^ build_system );
          ( "uv.lock",
            "version=1\n\
             requires-python='>=3.11'\n\
             [[package]]\n\
             name='app'\n\
             source={editable='.'}\n" );
          (".python-version", "3.13");
          ("main.py", "");
        ]
        label
        (fun root ->
          let prepared = ok (prepare root []) in
          let steps = steps prepared in
          let sync = "uv sync --locked --no-dev" in
          check "the venv comes from the stage that installs the project"
            (venv_follows_source prepared = finish);
          check "uv installs dependencies from manifests"
            (ordered steps
               [
                 "manifest .python-version pyproject.toml uv.lock";
                 sync ^ " --no-install-project";
                 "source";
               ]);
          check "the project installs after the source copy"
            (List.exists
               (String.starts_with ~prefix:(sync ^ " --no-editable"))
               steps
            = finish);
          if finish then
            check "project sync follows the source copy"
              (ordered steps [ "source"; sync ^ " --no-editable" ])))
    [
      ( "uv packaged project installs dependencies first",
        "[build-system]\n\
         requires=['setuptools']\n\
         build-backend='setuptools.build_meta'\n",
        true );
      ("uv virtual project installs dependencies only", "", false);
    ];
  snapshot
    [
      ( "pyproject.toml",
        "[project]\n\
         name='app'\n\
         version='1.0.0'\n\
         dependencies=['idna==3.10']\n\
         [tool.poetry]\n\
         package-mode=false\n" );
      ("poetry.lock", "");
      ("main.py", "");
    ]
    "Poetry application installs dependencies first"
    (fun root ->
      check "manager installs from manifests"
        (ordered
           (steps (ok (prepare root [])))
           [ "manifest poetry.lock pyproject.toml"; "poetry install"; "source" ]));
  List.iter
    (fun (commands, expected, manifests) ->
      snapshot
        [ ("requirements.txt", "idna==3.10\n"); ("main.py", "") ]
        ("install commands " ^ commands)
        (fun root ->
          write root "railpack.json"
            ({|{"steps":{"install":{"commands":|} ^ commands ^ "}}}");
          let prepared =
            ok
              (prepare root
                 [
                   ( "environment",
                     `Assoc [ ("RAILPACK_INSTALL_CMD", `String "echo ignored") ]
                   );
                 ])
          in
          let steps = steps prepared in
          check "configured commands take their place" (ordered steps expected);
          check "configuration replaces the install command"
            (not (List.mem "/bin/sh -c echo ignored" steps));
          check "inheritance selects the dependency layout"
            (List.exists
               (String.starts_with ~prefix:"manifest")
               (all_steps prepared)
            = manifests)))
    [
      ( {|["python -c 'print(1)'", "..."]|},
        [
          "/bin/sh -c python -c 'print(1)'";
          "/bin/sh -c [ ! -e /opt/venv/.kiln-emulate ]";
          "source";
        ],
        true );
      ( {|["...", "python -c 'print(1)'"]|},
        [
          "/bin/sh -c [ ! -e /opt/venv/.kiln-emulate ]";
          "/bin/sh -c python -c 'print(1)'";
          "source";
        ],
        true );
      ( {|["python -c 'print(1)'"]|},
        (* The runtime copies /opt/venv, so replacing installation keeps it. *)
        [ "python -m venv"; "source"; "/bin/sh -c python -c 'print(1)'" ],
        false );
    ];
  snapshot
    [
      ( "pyproject.toml",
        "[project]\n\
         name='example'\n\
         version='1.0.0'\n\
         [project.scripts]\n\
         serve='example:main'\n" );
    ]
    "Python console entry point"
    (fun root ->
      let plan = ok (prepare root []) in
      check "entry point"
        (Option.map Build_plan.arguments (Build_plan.startup plan.Planner.plan)
        = Some [ "serve" ]));
  let manager_project manager =
    "[project]\n\
     name = 'manager-app'\n\
     version = '1.0.0'\n\
     requires-python = '>=3.11,<3.14'\n\
     [project.scripts]\n\
     serve = 'manager_app:main'\n\
     [tool." ^ manager ^ "]\n"
  in
  List.iter
    (fun (manager, lockfile, default_version, install_command, lock_command) ->
      snapshot
        [
          ("pyproject.toml", manager_project manager);
          (lockfile, "");
          ("main.py", "");
        ]
        (manager ^ " selection and locked install plan")
        (fun root ->
          let prepared = ok (prepare root []) in
          let dockerfile = Planner.dockerfile prepared in
          check "manager lock validation is planned"
            (contains dockerfile lock_command);
          check "production manager install is planned"
            (contains dockerfile install_command);
          check "manager tool is installed in a separate virtualenv"
            (contains dockerfile "/opt/kiln-tools/bin/pip"
            && contains dockerfile (manager ^ "==" ^ default_version));
          check "manager tools are not copied into runtime"
            ((not (contains dockerfile "COPY --from=kiln-stage"))
            || not (contains dockerfile "/opt/kiln-tools/"));
          let open Yojson.Basic.Util in
          let package =
            `Assoc (Planner.info prepared)
            |> member "resolvedPackages" |> member manager
          in
          check "manager version is reported"
            (member "resolvedVersion" package = `String default_version
            && member "source" package = `String "kiln default");
          let metadata = `Assoc (Planner.info prepared) |> member "metadata" in
          check "manager metadata is reported"
            (member "pythonPackageManager" metadata = `String manager)))
    [
      ( "poetry",
        "poetry.lock",
        "2.5.1",
        "\"poetry\",\"install\",\"--only\",\"main\"",
        "\"poetry\",\"check\",\"--lock\"" );
      ( "pdm",
        "pdm.lock",
        "2.29.5",
        "\"pdm\",\"install\",\"--prod\",\"--no-editable\",\"--check\"",
        "\"pdm\",\"install\",\"--prod\",\"--no-editable\",\"--check\"" );
    ];
  List.iter
    (fun (manager, lockfile, no_root) ->
      snapshot
        [
          ( "pyproject.toml",
            manager_project manager
            ^
            if manager = "poetry" then "package-mode=false\n"
            else "distribution=false\n" );
          ("main.py", "");
        ]
        (manager ^ " unlocked install")
        (fun root ->
          let first = ok (prepare root []) in
          let dockerfile = Planner.dockerfile first in
          check "unlocked installs do not request lock checks"
            ((not (contains dockerfile "--check"))
            && not (contains dockerfile "check --lock"));
          check "unlocked installs do not copy project metadata back"
            (not (Sys.file_exists (Filename.concat root lockfile)));
          check "default manager command honors package mode"
            (contains dockerfile no_root)))
    [ ("poetry", "poetry.lock", "--no-root"); ("pdm", "pdm.lock", "--no-self") ];
  snapshot
    [
      ( "pyproject.toml",
        "[project]\n\
         name='manager-app'\n\
         version='1.0.0'\n\
         requires-python='>=3.11,<3.14'\n\
         [tool.poetry]\n\
         package-mode=true\n" );
      ("poetry.lock", "");
    ]
    "Poetry package mode and version overrides"
    (fun root ->
      let override =
        ok
          (prepare root
             [
               ( "environment",
                 `Assoc [ ("RAILPACK_POETRY_VERSION", `String "2.4.0") ] );
             ])
      in
      let open Yojson.Basic.Util in
      let package =
        `Assoc (Planner.info override)
        |> member "resolvedPackages" |> member "poetry"
      in
      check "exact manager override is reported"
        (member "resolvedVersion" package = `String "2.4.0"
        && member "source" package = `String "RAILPACK_POETRY_VERSION");
      check "Poetry package mode installs the root project"
        (contains
           (Planner.dockerfile override)
           "\"poetry\",\"install\",\"--only\",\"main\""));
  List.iter
    (fun (manager, extra, expected) ->
      snapshot
        [ ("pyproject.toml", manager_project manager ^ extra); ("main.py", "") ]
        (manager ^ " application mode")
        (fun root ->
          let dockerfile = Planner.dockerfile (ok (prepare root [])) in
          check "application mode skips installing the project itself"
            (contains dockerfile expected)))
    [
      ("poetry", "package-mode = false\n", "--no-root");
      ("pdm", "distribution = false\n", "--no-self");
    ];
  snapshot
    [
      ("pyproject.toml", manager_project "pdm" ^ "distribution=true\n");
      ("pdm.lock", "");
      ("main.py", "");
    ]
    "PDM version override"
    (fun root ->
      let open Yojson.Basic.Util in
      let package prepared =
        `Assoc (Planner.info prepared)
        |> member "resolvedPackages" |> member "pdm"
      in
      let override =
        ok
          (prepare root
             [
               ( "environment",
                 `Assoc [ ("RAILPACK_PDM_VERSION", `String "2.28.1") ] );
             ])
      in
      let package_override = package override in
      check "exact PDM override is reported"
        (member "resolvedVersion" package_override = `String "2.28.1"
        && member "source" package_override = `String "RAILPACK_PDM_VERSION"));
  snapshot
    [
      ( "pyproject.toml",
        "[project]\n\
         name='root-app'\n\
         version='1.0.0'\n\
         requires-python='>=3.11,<3.14'\n" );
      ("main.py", "");
    ]
    "nested package metadata does not select the application manager"
    (fun root ->
      Unix.mkdir (Filename.concat root "nested") 0o700;
      write
        (Filename.concat root "nested")
        "pyproject.toml"
        "[project]\nname='nested'\nversion='1.0.0'\n[tool.poetry]\n";
      let metadata =
        `Assoc (Planner.info (ok (prepare root [])))
        |> Yojson.Basic.Util.member "metadata"
      in
      check "root installer remains pip"
        (Yojson.Basic.Util.member "pythonPackageManager" metadata
        = `String "pip"));
  snapshot
    [
      ( "pyproject.toml",
        "[project]\n\
         name='manager-app'\n\
         version='1.0.0'\n\
         requires-python='>=3.11,<3.14'\n\
         dependencies=['local @ file:///${PROJECT_ROOT}/packages/local']\n\
         [tool.pdm]\n\
         distribution=false\n" );
      ("pdm.lock", "");
    ]
    "PDM project-root references stay contained"
    (fun root ->
      Unix.mkdir (Filename.concat root "packages") 0o700;
      Unix.mkdir (Filename.concat root "packages/local") 0o700;
      write
        (Filename.concat root "packages/local")
        "pyproject.toml" "[project]\nname='local'\nversion='1.0.0'\n";
      check "contained project-root dependency is accepted"
        (Result.is_ok (prepare root [])));
  snapshot
    [
      ( "pyproject.toml",
        "[project]\n\
         name='manager-app'\n\
         version='1.0.0'\n\
         requires-python='>=3.11,<3.14'\n\
         dependencies=['local @ file:///${PROJECT_ROOT}/../outside']\n\
         [tool.pdm]\n\
         distribution=false\n" );
      ("main.py", "");
    ]
    "PDM project-root escape is rejected"
    (fun root ->
      check "escaping local dependency is rejected"
        (Result.is_error (prepare root [])));
  List.iter
    (fun (label, files, message) ->
      snapshot files label (fun root ->
          match prepare root [] with
          | Error error ->
              check "actionable manager error"
                (contains (Kiln_core.Error.show error) message)
          | Ok _ -> check "manager metadata must be rejected" false))
    [
      ( "multiple manager lockfiles",
        [
          ("pyproject.toml", manager_project "poetry");
          ("poetry.lock", "");
          ("pdm.lock", "");
        ],
        "multiple lockfiles" );
      ( "ambiguous manager declarations",
        [
          ( "pyproject.toml",
            "[project]\nname='x'\nversion='1'\n[tool.poetry]\n[tool.pdm]\n" );
        ],
        "ambiguous Python package manager" );
      ( "legacy Poetry project is actionable",
        [
          ( "pyproject.toml",
            "[tool.poetry]\n\
             [tool.poetry.dependencies]\n\
             python='^3.11'\n\
             flask='*'\n" );
          ("poetry.lock", "");
        ],
        "modern [project] metadata" );
      ( "legacy Poetry dependencies require migration",
        [
          ( "pyproject.toml",
            "[project]\n\
             requires-python='>=3.11,<3.14'\n\
             [tool.poetry]\n\
             [tool.poetry.dependencies]\n\
             flask='*'\n" );
          ("poetry.lock", "");
        ],
        "migrate them to [project].dependencies" );
      ( "legacy Poetry Python constraint requires migration",
        [
          ( "pyproject.toml",
            "[project]\n\
             name='x'\n\
             version='1'\n\
             [tool.poetry]\n\
             [tool.poetry.dependencies]\n\
             python='^3.11'\n" );
          ("poetry.lock", "");
        ],
        "migrate them to [project].requires-python" );
      ( "PDM lock local path escape is rejected",
        [
          ("pyproject.toml", manager_project "pdm");
          ("pdm.lock", "[[package]]\npath = '/etc/passwd'\n");
        ],
        "path escapes or is missing" );
    ];
  List.iter
    (fun (label, files) ->
      snapshot files label (fun root ->
          check "rejected" (Result.is_error (prepare root []))))
    [
      ( "unsupported Python selector",
        [ ("main.py", ""); (".python-version", "pypy3.10") ] );
      ( "invalid TOML is not ignored",
        [
          ("main.py", ""); ("pyproject.toml", "[project]\nrequires-python = [\n");
        ] );
      ( "wrong metadata type",
        [
          ("main.py", "");
          ("pyproject.toml", "[project]\nrequires-python = 312\n");
        ] );
      ( "requirements escape",
        [ ("main.py", ""); ("requirements.txt", "-r /etc/passwd\n") ] );
      ( "cyclic requirements",
        [ ("main.py", ""); ("requirements.txt", "-r ./requirements.txt\n") ] );
      ( "local dependency escape",
        [ ("main.py", ""); ("requirements.txt", "local@file:///etc/passwd\n") ]
      );
      ("uv requires project", [ ("main.py", ""); ("uv.lock", "version=1\n") ]);
    ];
  finish ()
