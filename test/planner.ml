module Build_plan = Kiln_planning.Build_plan
module Json = Kiln_planning.Json
open Contract_support

let files = [ ("package.json", {|{"main":"main.js"}|}); ("main.js", "") ]
let recipe root fields = Build_plan.dockerfile (ok (prepare root fields))

let () =
  List.iter
    (fun (provider, files) ->
      snapshot files (provider ^ " optional startup") (fun root ->
          check "inherited image command cleared"
            (contains (recipe root []) "CMD []")))
    [
      ("Node", [ ("package.json", "{}") ]);
      ("empty Node entry", [ ("package.json", {|{"main":""}|}) ]);
      ("Bun", [ ("package.json", {|{"packageManager":"bun@1"}|}) ]);
      ("Deno", [ ("deno.json", "{}") ]);
      ("Python", [ ("requirements.txt", "") ]);
    ];
  snapshot
    [
      ("requirements.txt", "");
      ("main.py", "");
      ("package.json", {|{"main":"main.js","scripts":{"build":"vite build"}}|});
    ]
    "a railpack.json is not read"
    (fun root ->
      write root "railpack.json" {|{"provider":"node"}|};
      check "the source's own provider plans it"
        (contains (recipe root []) "docker.io/library/python@"));
  List.iter
    (fun (provider, image, files) ->
      let page =
        {|{"packageManager":"pnpm@11.25.0","scripts":{"build":"vite build"}}|}
      in
      snapshot (("package.json", page) :: files)
        (provider ^ " builds a page that declares its package manager")
        (fun root ->
          let recipe = recipe root [] in
          check "the app's own provider plans it"
            (contains recipe ("docker.io/library/" ^ image ^ "@"));
          check "with the page built"
            (contains recipe {|RUN ["pnpm","run","build"]|})))
    [
      ("python", "python", [ ("requirements.txt", ""); ("main.py", "") ]);
      ( "go",
        "golang",
        [
          ("go.mod", "module example\ngo 1.27\n"); ("main.go", "package main\n");
        ] );
    ];
  List.iter
    (fun (name, files, go) ->
      snapshot (("go.mod", "module example\ngo 1.27\n") :: files) name
        (fun root ->
          let recipe = recipe root [] in
          check
            (if go then "with Go's image" else "with Node's image and not Go's")
            (contains recipe "docker.io/library/golang@" = go
            && (go || contains recipe "docker.io/library/node@"))))
    [
      ("a root main package is Go", [ ("main.go", "package main\n") ], true);
      ( "a main package beside a constrained tools file is Go",
        [
          ("main.go", "package main\n");
          ("tools.go", "//go:build tools\n\npackage tools\n");
        ],
        true );
      ( "a main package built only for Linux is Go",
        [ ("server_linux.go", "//go:build linux\n\npackage main\n") ],
        true );
      ( "a Go library beside an ignored generator and a Node server plans as \
         Node",
        [
          ( "library.go",
            "// Package library is imported, not run.\npackage library\n" );
          ("generate.go", "//go:build ignore\n\npackage main\n");
          ("server.js", "");
          ("package.json", {|{"scripts":{"build":"vite build"}}|});
        ],
        false );
    ];
  snapshot files "the C API returns the recipe" (fun root ->
      check "its dockerfile alone"
        (Yojson.Basic.from_string (ok (Kiln_io.Driver.plan (request root [])))
        = `Assoc [ ("dockerfile", `String (recipe root [])) ]));
  snapshot [] "unknown application" (fun root ->
      check "no implicit application fixture"
        (Result.is_error (prepare root [])));
  snapshot files "native startup" (fun root ->
      let argv = [ "/launcher"; "with spaces"; "\"quoted\""; "" ] in
      check
        "native launcher argv overrides application startup without shell \
         interpretation"
        (contains
           (recipe root
              [ ("image", `Assoc [ ("start", Json.string_array argv) ]) ])
           ("CMD " ^ Json.encode (Json.string_array argv))));
  snapshot files "invalid request is rejected" (fun root ->
      List.iter
        (fun fields ->
          check "invalid request rejected"
            (Result.is_error (prepare root fields)))
        [
          [ ("image", `Assoc [ ("start", `List []) ]) ];
          [ ("image", `Assoc [ ("start", `List [ `String "" ]) ]) ];
          [
            ( "image",
              `Assoc [ ("start", `List [ `String "node"; `String "\000" ]) ] );
          ];
          [ ("image", `Assoc []); ("development", `Assoc []) ];
        ]);
  snapshot
    [
      ("requirements.txt", "");
      ("main.py", "");
      ("package.json", {|{"scripts":{"build":"vite build"}}|});
    ]
    "a run executes a command in the image without its build"
    (fun root ->
      let run fields =
        prepare root
          [
            ( "run",
              `Assoc
                ([
                   ("command", Json.string_array [ "python"; "check.py" ]);
                   ("mounts", `Assoc [ ("tooling", `String "/tooling") ]);
                   ("overlay", `String "generated");
                 ]
                @ fields) );
          ]
      in
      let dockerfile =
        Build_plan.dockerfile
          (ok (run [ ("packages", Json.string_array [ "mypy==2.4.0" ]) ]))
      in
      check "the app's page is not built"
        (not (contains dockerfile {|RUN ["npm","run","build"]|}));
      check
        "the packages install apart, the overlay lands on the app, and the \
         command runs with no network and its mounts"
        (ordered
           (String.split_on_char '\n' dockerfile)
           [
             {|RUN ["python","-m","pip","install"|};
             {|COPY --from=generated [".","/app/"]|};
             {|ENV PYTHONPATH="/opt/kiln-packages"|};
             {|RUN --network=none --mount=type=bind,from=tooling,target=/tooling,readonly ["/bin/sh","-c",|};
             "FROM scratch";
           ]);
      check "the command is passed as arguments"
        (contains dockerfile {|"sh","python","check.py"]|});
      write root "package.json" {|{"main":"main.js"}|};
      Sys.remove (Filename.concat root "requirements.txt");
      Sys.remove (Filename.concat root "main.py");
      check "packages need Python"
        (Result.is_error (run [ ("packages", Json.string_array [ "mypy" ]) ])));
  let page = {|{"scripts":{"build":"vite build"}}|} in
  let python =
    [
      ( "pyproject.toml",
        "[project]\n\
         name = \"app\"\n\
         version = \"1\"\n\
         requires-python = \">=3.12\"\n" );
      ("server.py", "");
    ]
  in
  let uv_lock = ("uv.lock", "version = 1\nrequires-python = \">=3.12\"\n") in
  let develop root fields =
    Result.map
      (fun response -> Yojson.Basic.from_string response)
      (Kiln_io.Driver.plan (request root [ ("development", `Assoc fields) ]))
  in
  let start = [ ("start", Json.string_array [ "serve"; "server.py" ]) ] in
  let digest plan =
    Yojson.Basic.Util.(plan |> member "install" |> member "inputs")
  in
  let without_digest = function
    | `Assoc (("install", `Assoc (("inputs", _) :: install)) :: plan) ->
        `Assoc (("install", `Assoc install) :: plan)
    | plan -> plan
  in
  snapshot files "a development request names its start" (fun root ->
      check "rejected without one" (Result.is_error (develop root [])));
  List.iter
    (fun (name, files, expected) ->
      snapshot files ("development plan: " ^ name) (fun root ->
          match (develop root start, expected) with
          | Ok plan, Some (inputs, expected) ->
              check "the plan"
                (without_digest plan = Yojson.Basic.from_string expected);
              List.iter
                (fun (file, contents) ->
                  let before = digest (ok (develop root start)) in
                  write root file (contents ^ "\n");
                  check
                    (file ^ " changes the install's inputs only if one")
                    (digest (ok (develop root start))
                    <> before = List.mem file inputs))
                files
          | Error _, None -> ()
          | Ok plan, None -> failwith ("planned: " ^ Yojson.Basic.to_string plan)
          | Error error, Some _ -> failwith (Kiln_planning.Error.show error)))
    [
      ( "pnpm leaves its lockfile unfrozen, for an image Kiln cannot plan",
        [
          ( "package.json",
            {|{"engines":{"node":">=99"},"scripts":{"build":"vite build"}}|} );
          ("pnpm-lock.yaml", "");
        ],
        Some
          ( [ "package.json"; "pnpm-lock.yaml" ],
            {|{"install":{"commands":[["pnpm","install"]]},"prepare":[["pnpm","run","build"]],"start":["serve","server.py"]}|}
          ) );
      ( "uv with a page, reinstalled when a manifest changes",
        uv_lock :: ("package.json", page)
        :: (".python-version", "3.13\n")
        :: (".npmrc", "") :: python,
        Some
          ( [
              ".python-version";
              "pyproject.toml";
              "uv.lock";
              ".npmrc";
              "package.json";
            ],
            {|{"install":{"commands":[["uv","sync"],["npm","install"]]},"prepare":[["npm","run","build"]],"start":["uv","run","--no-sync","serve","server.py"]}|}
          ) );
      ( "uv alone",
        uv_lock :: python,
        Some
          ( [ "pyproject.toml"; "uv.lock" ],
            {|{"install":{"commands":[["uv","sync"]]},"prepare":[],"start":["uv","run","--no-sync","serve","server.py"]}|}
          ) );
      ("Python without uv.lock", python, None);
      ( "Go whose package.json declares its package manager and builds nothing",
        [
          ("go.mod", "module example\ngo 1.27\n");
          ("main.go", "package main\n");
          ( "package.json",
            {|{"packageManager":"pnpm@11.25.0","dependencies":{"holon-sh":"^0.0.1"}}|}
          );
        ],
        Some
          ( [ "package.json" ],
            {|{"install":{"commands":[["pnpm","install"]]},"prepare":[],"start":["serve","server.py"]}|}
          ) );
      ( "Node beside a Python version file",
        [ ("package.json", "{}"); (".python-version", "3.13\n") ],
        Some
          ( [ "package.json" ],
            {|{"install":{"commands":[["npm","install"]]},"prepare":[],"start":["serve","server.py"]}|}
          ) );
      ("Bun", [ ("package.json", {|{"packageManager":"bun@1"}|}) ], None);
    ];
  finish ()
