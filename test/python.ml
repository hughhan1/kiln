module Planner = Kiln_planning.Planner
module Build_plan = Kiln_planning.Build_plan
open Contract_support

let fetch =
  registry
    [
      ( "library/python",
        List.concat_map
          (fun version -> [ version ^ "-trixie"; version ^ "-slim-trixie" ])
          [
            "3.11.14";
            "3.12.0";
            "3.12.11";
            "3.12.12";
            "3.13.7";
            "3.14.0rc1";
            "4.0.0";
          ]
        @ [ "3.14.0-alpine" ] );
      ("astral/uv", [ "0.8.0"; "0.9.1"; "0.10.0-rc.1" ]);
    ]

let prepare ?(fetch = fetch) root fields =
  Kiln_io.Driver.prepare_with ~today:"2026-10-12" ~fetch (request root fields)

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

let steps prepared =
  List.find (List.mem "source")
    (List.map
       (fun stage -> List.filter_map step (Build_plan.operations stage))
       (Build_plan.stages prepared.Planner.plan))

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
    (fun (constraint_, versions, expected) ->
      snapshot
        [
          ("main.py", "");
          ( "pyproject.toml",
            "[project]\nrequires-python = '" ^ constraint_ ^ "'\n" );
        ]
        ("Python requirement " ^ constraint_ ^ ": " ^ String.concat "," versions)
        (fun root ->
          let fetch =
            registry
              [
                ( "library/python",
                  List.concat_map
                    (fun version ->
                      [ version ^ "-trixie"; version ^ "-slim-trixie" ])
                    versions );
              ]
          in
          let result = prepare ~fetch root [] in
          match expected with
          | Some version ->
              check "highest matching stable interpreter"
                (selected (ok result) = `String version)
          | None ->
              check "no matching interpreter"
                (Result.map selected result
                = Error
                    (Kiln_core.Error.No_version
                       {
                         tool = "library/python";
                         requirements = "declared version constraints";
                       }))))
    [
      (">=3.11,<3.14,!=3.12.*", [ "3.11.14"; "3.13.7" ], Some "3.13.7");
      (">=3.11,<3.14,!=3.12.*", [ "3.10.9"; "3.12.12"; "3.14.0" ], None);
      ("~=3.12.1", [ "3.12.1"; "3.12.12" ], Some "3.12.12");
      ("~=3.12.1", [ "3.12.0"; "3.13.0" ], None);
      ("~=3.12", [ "3.12.0"; "3.13.7" ], Some "3.13.7");
      ("~=3.12", [ "3.11.0"; "4.0.0" ], None);
      ("==3.12", [ "3.12.0"; "3.12.1" ], Some "3.12.0");
      ("==3.12,>3.12", [ "3.12.0"; "3.12.1" ], None);
      ("<=3.12.11,>3.11.14", [ "3.12.0"; "3.12.11" ], Some "3.12.11");
      ("<=3.12.11,>3.11.14", [ "3.11.14"; "3.12.12" ], None);
    ];
  snapshot
    [
      ("main.py", "print('hello')");
      (".python-version", "3.12");
      ("pyproject.toml", "[project]\nrequires-python = '>=3.11,!=3.12.12'\n");
    ]
    "Python intersects file and project constraints"
    (fun root ->
      let first = ok (prepare root []) in
      check "does not truncate a range" (selected first = `String "3.12.11");
      write root "main.py" "print('changed')";
      let reused =
        ok
          (prepare
             ~fetch:(fun _ _ -> failwith "offline discovery")
             root
             [ ("resolution", first.Planner.resolution) ])
      in
      check "toolchain unchanged"
        (reused.Planner.resolution = first.Planner.resolution);
      let open Yojson.Basic.Util in
      let package =
        `Assoc (Planner.info reused)
        |> member "resolvedPackages" |> member "python"
      in
      check "reused toolchain retains the intersected request and its sources"
        (member "requestedVersion" package = `String "3.12 && >=3.11,!=3.12.12"
        && member "source" package
           = `String ".python-version, pyproject.toml project.requires-python");
      write root ".python-version" "3.13";
      check "requirements invalidate reuse"
        (selected
           (ok (prepare root [ ("resolution", first.Planner.resolution) ]))
        = `String "3.13.7"));
  snapshot
    [
      ("main.py", "");
      (".python-version", "3.12.11");
      ("pyproject.toml", "[project]\nrequires-python = '>=3.11'\n");
      ("uv.lock", "version=1\nrequires-python = '>=3.11'\n");
    ]
    "uv defaults, previous versions, and intersected declarations"
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
        && member "resolvedVersion" default = `String "0.9.1");
      let previous = ("previousVersions", `Assoc [ ("uv", `String "0.8.0") ]) in
      let pinned =
        ok
          (prepare
             ~fetch:(fun url token ->
               check "exact toolchain avoids catalogs"
                 (not (String.ends_with ~suffix:"/tags/list" url));
               fetch url token)
             root [ previous ])
      in
      let uv = package pinned in
      check "previous version is preserved as declared"
        (member "requestedVersion" uv = `String "0.8.0"
        && member "resolvedVersion" uv = `String "0.8.0"
        && member "source" uv = `String "previousVersions");
      write root "uv.toml" "required-version = '>=0.9'\n";
      write root "pyproject.toml"
        "[project]\n\
         requires-python = '>=3.11'\n\
         [tool.uv]\n\
         required-version = '<0.10'\n";
      let first = ok (prepare root [ previous ]) in
      let uv = package first in
      check
        "explicit constraints supersede previous defaults with their origins"
        (member "requestedVersion" uv = `String "<0.10 && >=0.9"
        && member "resolvedVersion" uv = `String "0.9.1"
        && member "source" uv
           = `String
               "pyproject.toml tool.uv.required-version, uv.toml \
                required-version"));
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
                     ("RAILPACK_PYTHON_VERSION", `String "3.12.11");
                     ("RAILPACK_UV_VERSION", `String "0.8.0");
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
          ("python", "3.12.11", "RAILPACK_PYTHON_VERSION");
          ("uv", "0.8.0", "RAILPACK_UV_VERSION");
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
      let steps = steps prepared in
      check "environment, manifests, install, then source"
        (ordered steps
           [
             "python -m venv";
             "manifest requirements.txt";
             "manifest requirements/base.txt";
             "python -m pip install --no-cache-dir -r requirements.txt";
             "source";
           ]);
      let dockerfile = Planner.dockerfile prepared in
      check "manifests keep their own paths"
        (contains dockerfile {|COPY ["requirements.txt","./"]|}
        && contains dockerfile
             {|COPY ["requirements/base.txt","requirements/"]|}));
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
      ( "project install keeps the whole source",
        [
          ("requirements.txt", "idna==3.10\n");
          ( "pyproject.toml",
            "[project]\n\
             name='app'\n\
             version='1.0.0'\n\
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
          let steps = steps (ok (prepare root [])) in
          let sync = "uv sync --locked --no-dev" in
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
        "2.29.2",
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
        (manager ^ " unlocked install and version reuse")
        (fun root ->
          let first = ok (prepare root []) in
          let dockerfile = Planner.dockerfile first in
          check "unlocked installs do not request lock checks"
            ((not (contains dockerfile "--check"))
            && not (contains dockerfile "check --lock"));
          check "unlocked installs do not copy project metadata back"
            (not (Sys.file_exists (Filename.concat root lockfile)));
          check "default manager command honors package mode"
            (contains dockerfile no_root);
          let reused =
            ok
              (prepare
                 ~fetch:(fun _ _ ->
                   failwith "manager resolution discovered tools")
                 root
                 [ ("resolution", first.Planner.resolution) ])
          in
          check "manager resolution is reused without registry discovery"
            (reused.Planner.resolution = first.Planner.resolution)))
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
           "\"poetry\",\"install\",\"--only\",\"main\"");
      let previous =
        ok
          (prepare root
             [ ("previousVersions", `Assoc [ ("poetry", `String "2.3.0") ]) ])
      in
      let package =
        `Assoc (Planner.info previous)
        |> member "resolvedPackages" |> member "poetry"
      in
      check "previous manager version is selected"
        (member "resolvedVersion" package = `String "2.3.0"
        && member "source" package = `String "previousVersions"));
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
    "PDM version override and previous version"
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
        && member "source" package_override = `String "RAILPACK_PDM_VERSION");
      let previous =
        ok
          (prepare root
             [ ("previousVersions", `Assoc [ ("pdm", `String "2.27.0") ]) ])
      in
      let package_previous = package previous in
      check "previous PDM version is selected"
        (member "resolvedVersion" package_previous = `String "2.27.0"
        && member "source" package_previous = `String "previousVersions"));
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
