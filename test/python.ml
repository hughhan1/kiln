module Build_plan = Kiln_planning.Build_plan
module Pins = Kiln_providers.Pins
module Version = Kiln_providers.Python_version
open Contract_support

let oldest_python = (List.hd Pins.python).Pins.release
let newest_python = (last Pins.python).Pins.release
let oldest_uv = List.hd Pins.uv
let dockerfile root = Build_plan.dockerfile (ok (prepare root []))

(* Builder steps as text: context copies and RUN argv. *)
let step = function
  | Build_plan.Copy { input = Build_plan.Context; paths = [ "." ]; _ } ->
      Some "source"
  | Build_plan.Copy { input = Build_plan.Context; paths; _ } ->
      Some ("manifest " ^ String.concat " " paths)
  | Build_plan.Run argv -> Some (String.concat " " (Build_plan.arguments argv))
  | Build_plan.Copy { input = Build_plan.Output _ | Build_plan.External _; _ }
  | Build_plan.Workdir _ | Build_plan.Env _ | Build_plan.User _
  | Build_plan.Entrypoint | Build_plan.Start _ | Build_plan.Isolated _ ->
      None

let own_steps stage = List.filter_map step (Build_plan.operations stage)

(* Steps of a stage preceded by those of the stages it is built from. *)
let rec inherited_steps stage =
  (match Build_plan.base stage with
    | Build_plan.Stage parent -> inherited_steps parent
    | Build_plan.Image _ | Build_plan.Native _ | Build_plan.Scratch -> [])
  @ own_steps stage

let steps plan =
  inherited_steps
    (List.find
       (fun stage -> List.mem "source" (own_steps stage))
       (List.rev (Build_plan.stages plan)))

(* Steps of every stage, each after the stages it depends on. *)
let all_steps plan = List.concat_map own_steps (Build_plan.stages plan)

(* Whether the runtime copies the venv from a stage that copies the source. *)
let venv_follows_source plan =
  List.exists
    (function
      | Build_plan.Copy { input; paths = [ "/opt/venv/" ]; _ } -> (
          match input with
          | Build_plan.Output stage -> List.mem "source" (own_steps stage)
          | Build_plan.Context | Build_plan.External _ -> false)
      | Build_plan.Copy _ | Build_plan.Run _ | Build_plan.Workdir _
      | Build_plan.Env _ | Build_plan.User _ | Build_plan.Entrypoint
      | Build_plan.Start _ | Build_plan.Isolated _ ->
          false)
    (Build_plan.operations plan)

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
  snapshot
    [
      ("main.py", "print('hello')");
      (".python-version", oldest_python);
      ( "pyproject.toml",
        "[project]\nrequires-python = '>=3,!=" ^ newest_python ^ "'\n" );
    ]
    "Python intersects file and project constraints"
    (fun root ->
      check "the intersection selects the oldest pin"
        (contains (dockerfile root) (List.hd Pins.python).Pins.runtime);
      write root ".python-version" newest_python;
      check "declarations that exclude each other leave nothing"
        (Result.is_error (prepare root [])));
  snapshot
    [
      ("main.py", "");
      (".python-version", newest_python);
      ("pyproject.toml", "[project]\nrequires-python = '>=3.11'\n");
      ("uv.lock", "version=1\nrequires-python = '>=3.11'\n");
    ]
    "uv intersects declarations"
    (fun root ->
      write root "uv.toml"
        ("required-version = '>=" ^ oldest_uv.Pins.version ^ "'\n");
      write root "pyproject.toml"
        ("[project]\n\
          requires-python = '>=3.11'\n\
          [tool.uv]\n\
          required-version = '<=" ^ oldest_uv.Pins.version ^ "'\n");
      check "tool.uv.required-version picks the oldest uv image"
        (contains (dockerfile root) oldest_uv.Pins.image));
  snapshot
    [
      ("requirements.txt", "idna==3.10\n-r requirements/base.txt\n");
      ("main.py", "");
    ]
    "remote requirements install before the source copy"
    (fun root ->
      Unix.mkdir (Filename.concat root "requirements") 0o700;
      write root "requirements/base.txt" "--hash=sha256:abc\nflask>=3\n";
      let plan = ok (prepare root []) in
      check "the venv comes from the stage before the source copy"
        (not (venv_follows_source plan));
      check "environment, manifests, native install, fallback, then source"
        (ordered (all_steps plan)
           [
             "python -m venv";
             "manifest requirements.txt";
             "manifest requirements/base.txt";
             "/bin/sh -c case";
             "/bin/sh -c [ ! -e /opt/venv/.kiln-emulate ]";
             "source";
           ]);
      let dockerfile = Build_plan.dockerfile plan in
      check "the fallback installs the same requirements"
        (contains dockerfile {|"kiln","-r","requirements.txt"|});
      check "manifests keep their own paths"
        (contains dockerfile {|COPY ["requirements.txt","./"]|}
        && contains dockerfile
             {|COPY ["requirements/base.txt","requirements/"]|}));
  let frontend package test checks =
    snapshot
      [
        ("requirements.txt", "idna==3.10\n");
        ("main.py", "");
        ("package.json", package);
      ]
      test
      (fun root -> checks (dockerfile root))
  in
  frontend {|{"scripts":{"build":"vite build"}}|}
    "a package.json build installs before the source on the build platform"
    (fun dockerfile ->
      check "manifests, install, source without Python's files, then build"
        (ordered
           (String.split_on_char '\n' dockerfile)
           [
             "FROM --platform=$BUILDPLATFORM docker.io/library/node";
             {|COPY ["package.json","./"]|};
             {|RUN ["npm","install"]|};
             "COPY --exclude=";
             {|RUN ["npm","run","build"]|};
           ]);
      check "Python's manifests stay out of the frontend"
        (contains dockerfile "--exclude=requirements.txt");
      check "node_modules stays out of the image"
        (contains dockerfile {|--exclude=**/node_modules ["/app/","/app/"]|}));
  frontend {|{"devDependencies":{"prettier":"3.0.0"}}|}
    "a package.json without a build script builds nothing" (fun dockerfile ->
      check "no Node stage" (not (contains dockerfile "npm")));
  let pyproject dependency =
    "[project]\nname='app'\ndynamic=['version']\ndependencies=['" ^ dependency
    ^ "']\n\
       [build-system]\n\
       requires=['setuptools']\n\
       build-backend='setuptools.build_meta'\n"
  in
  let project = pyproject "idna==3.10" in
  snapshot
    [ ("pyproject.toml", project); ("main.py", "") ]
    "pip project installs dependencies natively before the source"
    (fun root ->
      let plan = ok (prepare root []) in
      let dockerfile = Build_plan.dockerfile plan in
      check "the install stage runs on the build platform and targets the image"
        (contains dockerfile "FROM --platform=$BUILDPLATFORM"
        && contains dockerfile "ARG TARGETARCH"
        && contains dockerfile {|--python-platform \"$arch-manylinux_2_40\"|}
        && contains dockerfile {|"idna==3.10"]|});
      check "the project installs alone after the source copy"
        (ordered (steps plan)
           [
             "/bin/sh -c [ ! -e /opt/venv/.kiln-emulate ]";
             "source";
             "/bin/sh -c set -e; [ ! -e /opt/kiln-project/.kiln-emulate ]";
           ]);
      check "the project builds natively too"
        (contains dockerfile {|"--no-deps","--prefix=/opt/kiln-project","."|});
      check "a source edit leaves the venv layer alone"
        (not (venv_follows_source plan));
      check "the runtime overlays the project and compiles the standard library"
        (contains dockerfile {|["/opt/kiln-project/","/opt/venv/"]|}
        && contains dockerfile {|"compileall","-q","-j","0","/usr/local/lib"|});
      write root "main.py" "print('edited')\n";
      write root "pyproject.toml" (project ^ "[tool.ruff]\nline-length=100\n");
      check "editing source or unrelated configuration keeps the plan"
        (Build_plan.dockerfile (ok (prepare root [])) = dockerfile);
      write root "pyproject.toml" (pyproject "idna==3.11");
      check "a dependency bump changes the install"
        (Build_plan.dockerfile (ok (prepare root [])) <> dockerfile));
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
          let plan = ok (prepare root []) in
          let steps = steps plan in
          let sync = "uv sync --locked --no-dev" in
          let project = sync ^ " --no-editable" in
          check "the venv comes from the stage that installs the project"
            (venv_follows_source plan = finish);
          check
            "dependencies install from manifests, the project after the source"
            (ordered steps
               ([
                  "manifest .python-version pyproject.toml uv.lock";
                  sync ^ " --no-install-project";
                  "source";
                ]
               @ if finish then [ project ] else []));
          check "a virtual project has nothing to install"
            (finish
            || not (List.exists (String.starts_with ~prefix:project) steps))))
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
      check "entry point" (contains (dockerfile root) {|CMD ["serve"]|}));
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
    (fun (manager, lockfile, tool_requirement, install_command, lock_command) ->
      snapshot
        [
          ("pyproject.toml", manager_project manager);
          (lockfile, "");
          ("main.py", "");
        ]
        (manager ^ " selection and locked install plan")
        (fun root ->
          let dockerfile = dockerfile root in
          check "manager lock validation is planned"
            (contains dockerfile lock_command);
          check "production manager install is planned"
            (contains dockerfile install_command);
          check "manager tool is installed in a separate virtualenv"
            (contains dockerfile "/opt/kiln-tools/bin/pip"
            && contains dockerfile tool_requirement)))
    [
      ( "poetry",
        "poetry.lock",
        "poetry==2.5.1",
        "\"poetry\",\"install\",\"--only\",\"main\"",
        "\"poetry\",\"check\",\"--lock\"" );
      ( "pdm",
        "pdm.lock",
        "pdm[locked]==2.29.7",
        "\"pdm\",\"install\",\"--prod\",\"--no-editable\"",
        "\"--check\"" );
    ];
  List.iter
    (fun (manager, no_root, lock_check) ->
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
          let dockerfile = dockerfile root in
          check "unlocked installs do not request lock checks"
            (not (contains dockerfile lock_check));
          check "default manager command honors package mode"
            (contains dockerfile no_root)))
    [
      ("poetry", "--no-root", {|"poetry","check","--lock"|});
      ("pdm", "--no-self", {|"--check"|});
    ];
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
      check "root installer remains pip"
        (not (contains (dockerfile root) "poetry")));
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
  List.iter
    (fun (label, files, message) ->
      snapshot files label (fun root ->
          match prepare root [] with
          | Error error ->
              check "actionable manager error"
                (contains (Kiln_planning.Error.show error) message)
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
      ( "PDM project-root escape",
        [
          ("main.py", "");
          ( "pyproject.toml",
            "[project]\n\
             name='manager-app'\n\
             version='1.0.0'\n\
             requires-python='>=3.11,<3.14'\n\
             dependencies=['local @ file:///${PROJECT_ROOT}/../outside']\n\
             [tool.pdm]\n\
             distribution=false\n" );
        ] );
    ];
  finish ()
