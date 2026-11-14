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
    "provider selection"
    (fun root ->
      write root "railpack.json" {|{"provider":"node"}|};
      check "configured provider selected"
        (not (contains (recipe root []) "docker.io/library/python@"));
      write root "railpack.json" {|{"provider":"unknown"}|};
      check "unknown provider rejected" (Result.is_error (prepare root [])));
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
      ("go", "golang", [ ("go.mod", "module example\ngo 1.27\n") ]);
    ];
  snapshot files "the C API returns the recipe" (fun root ->
      check "its dockerfile alone"
        (Yojson.Basic.from_string (ok (Kiln_io.Driver.plan (request root [])))
        = `Assoc [ ("dockerfile", `String (recipe root [])) ]));
  snapshot [] "unknown application" (fun root ->
      check "no implicit application fixture"
        (Result.is_error (prepare root [])));
  snapshot
    (files
    @ [
        ( "railpack.json",
          {|{
    // Configuration in JSONC syntax.
    "provider": "node",
  }|}
        );
      ])
    "native startup and JSONC configuration"
    (fun root ->
      let argv = [ "/launcher"; "with spaces"; "\"quoted\""; "" ] in
      check
        "native launcher argv overrides application startup without shell \
         interpretation"
        (contains
           (recipe root
              [ ("image", `Assoc [ ("start", Json.string_array argv) ]) ])
           ("CMD " ^ Json.encode (Json.string_array argv))));
  snapshot
    [
      ( "package.json",
        {|{"main":"main.js","scripts":{"build":"echo inherited"}}|} );
    ]
    "build command inheritance"
    (fun root ->
      List.iter
        (fun (commands, inherited) ->
          write root "railpack.json"
            ("{\"steps\":{\"build\":{" ^ commands ^ "}}}");
          check commands
            (contains (recipe root []) {|RUN ["npm","run","build"]|} = inherited))
        [
          ("", true);
          ({|"commands":[]|}, false);
          ({|"commands":["..."]|}, true);
          ({|"commands":[{"cmd":"..."}]|}, true);
          ({|"commands":[{"cmd":"sh -c '...'"}]|}, true);
        ]);
  snapshot files "inherited install follows earlier commands" (fun root ->
      write root "railpack.json"
        {|{"steps":{"install":{"commands":["echo before","..."]}}}|};
      let recipe = recipe root [] in
      let position line = Option.get (String.find_first ~sub:line recipe) in
      check "inherited install follows earlier commands"
        (position {|RUN ["/bin/sh","-c","echo before"]|}
        < position {|"npm","install"]|}));
  snapshot files "invalid configuration is rejected" (fun root ->
      let reject config =
        write root "railpack.json" config;
        check config (Result.is_error (prepare root []))
      in
      List.iter reject
        [
          {|{"deploy":{"startCommand":"echo start"}}|};
          {|{"steps":{"build":{"inputs":[]}}}|};
          {|{"steps":{"build":{"commands":[{"cmd":"'unclosed"}]}}}|};
          {|{"steps":{"build":{"commands":[{"cmd":"echo ignored","path":"/tools"}]}}}|};
          {|{"steps":{"build":{"commands":["COPY:only-source"]}}}|};
          {|{"steps":{"build":{"commands":[{"src":"","dest":"copy.js"}]}}}|};
          {|{"steps":{"build":{"commands":[{"src":"main.js","dest":"copy.js","image":"bad image"}]}}}|};
          {|{"steps":{"build":{"commands":["PATH:/tools\rINJECT"]}}}|};
          {|{"steps":{"build":{"commands":[" "]}}}|};
        ]);
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
  snapshot files "installed packages stay out of the source" (fun root ->
      Unix.mkdir (Filename.concat root "node_modules") 0o700;
      write root "node_modules/package.json" "[";
      Unix.mkdir (Filename.concat root ".venv") 0o700;
      Unix.symlink "/usr/bin/env" (Filename.concat root ".venv/python");
      let recipe = recipe root [] in
      check "they are neither read nor copied"
        (contains recipe "--exclude=**/node_modules "
        && contains recipe "--exclude=**/.venv "));
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
  List.iter
    (fun (name, files, expected) ->
      snapshot files ("development steps: " ^ name) (fun root ->
          let planned =
            Kiln_io.Driver.plan (request root [ ("development", `Assoc []) ])
          in
          match (planned, expected) with
          | Ok response, Some steps ->
              check "the steps"
                (Yojson.Basic.from_string response
                = Yojson.Basic.from_string steps)
          | Error _, None -> ()
          | Ok response, None -> failwith ("planned: " ^ response)
          | Error error, Some _ -> failwith (Kiln_planning.Error.show error)))
    [
      ( "pnpm leaves its lockfile unfrozen, for an image Kiln cannot plan",
        [
          ( "package.json",
            {|{"engines":{"node":">=99"},"scripts":{"build":"vite build"}}|} );
          ("pnpm-lock.yaml", "");
        ],
        Some
          {|{"install":[["pnpm","install"]],"build":[["pnpm","run","build"]]}|}
      );
      ( "uv with a page, whose railpack.json does not apply",
        uv_lock :: ("package.json", page)
        :: ( "railpack.json",
             {|{"steps":{"install":{"commands":["...",{"path":"/opt/node/bin"}]},"build":{"commands":["vite build"]}}}|}
           )
        :: python,
        Some
          {|{"install":[["uv","sync"],["npm","install"]],"build":[["npm","run","build"]]}|}
      );
      ( "uv alone",
        uv_lock :: python,
        Some {|{"install":[["uv","sync"]],"build":[]}|} );
      ("Python without uv.lock", python, None);
      ( "Go whose package.json declares its package manager and builds nothing",
        [
          ("go.mod", "module example\ngo 1.27\n");
          ( "package.json",
            {|{"packageManager":"pnpm@11.25.0","dependencies":{"holon-sh":"^0.0.1"}}|}
          );
        ],
        Some {|{"install":[["pnpm","install"]],"build":[]}|} );
      ("Bun", [ ("package.json", {|{"packageManager":"bun@1"}|}) ], None);
    ];
  finish ()
