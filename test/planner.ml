module Build_plan = Kiln_planning.Build_plan
module Json = Kiln_planning.Json
module Planner = Kiln_planning.Planner
module Provider = Kiln_planning.Provider
open Contract_support

let files = [ ("package.json", {|{"main":"main.js"}|}); ("main.js", "") ]
let recipe root fields = Planner.dockerfile (ok (prepare root fields))

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
  snapshot files "provider registration and selection" (fun root ->
      let module Alternate = struct
        include Kiln_providers.Node

        let id = "alternate"
        let public_name = id
        let detect _ = Ok Provider.No_match
      end in
      let providers : Provider.t list =
        [ (module Kiln_providers.Node); (module Alternate) ]
      in
      write root "railpack.json" {|{"provider":"alternate"}|};
      check "configured provider selected"
        (Yojson.Basic.Util.member "provider"
           (ok (prepare ~providers root [])).Planner.resolution
        = `String "alternate");
      write root "railpack.json" {|{"provider":"unknown"}|};
      check "unknown provider rejected" (Result.is_error (prepare root [])));
  List.iter
    (fun (provider, files) ->
      let page =
        {|{"packageManager":"pnpm@11.25.0","scripts":{"build":"vite build"}}|}
      in
      snapshot (("package.json", page) :: files)
        (provider ^ " builds a page that declares its package manager")
        (fun root ->
          let prepared = ok (prepare root []) in
          check "the app's own provider plans it"
            (Yojson.Basic.Util.member "provider" prepared.Planner.resolution
            = `String provider);
          check "with the page built"
            (contains
               (Planner.dockerfile prepared)
               {|RUN ["pnpm","run","build"]|})))
    [
      ("python", [ ("requirements.txt", ""); ("main.py", "") ]);
      ("go", [ ("go.mod", "module example\ngo 1.27\n") ]);
    ];
  snapshot [] "unknown application" (fun root ->
      check "no implicit application fixture"
        (Result.is_error (prepare root [])));
  test "startup inheritance" (fun () ->
      let image =
        ok (Build_plan.image ("example/image@sha256:" ^ String.make 64 'a'))
      in
      let args = [ "program" ] in
      let command = ok (Build_plan.argv args) in
      let parent =
        ok
          (Build_plan.stage (Build_plan.Image image)
             [ Build_plan.Start (Some command) ])
      in
      let other = ok (Build_plan.argv [ "other" ]) in
      List.iter
        (fun (label, operations, expected) ->
          let child =
            ok (Build_plan.stage (Build_plan.Stage parent) operations)
          in
          check label
            (Option.map Build_plan.arguments (Build_plan.startup child)
            = expected))
        [
          ("inherit startup", [], Some args);
          ( "entrypoint clears inherited CMD",
            [ Build_plan.Entrypoint None ],
            None );
          ( "entrypoint preserves local CMD",
            [ Build_plan.Start (Some other); Build_plan.Entrypoint None ],
            Some [ "other" ] );
          ( "last local CMD wins",
            [
              Build_plan.Entrypoint None;
              Build_plan.Start (Some command);
              Build_plan.Start (Some other);
            ],
            Some [ "other" ] );
        ]);
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
           (recipe root [ ("start", Json.string_array argv) ])
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
          [ ("start", `List []) ];
          [ ("start", `List [ `String "" ]) ];
          [ ("start", `List [ `String "node"; `String "\000" ]) ];
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
  snapshot files "contained custom configuration" (fun root ->
      Unix.mkdir (Filename.concat root "config") 0o700;
      write root "config/build.json"
        {|{"steps":{"build":{"commands":["echo nested"]}}}|};
      check "custom path used"
        (contains
           (recipe root [ ("configFilePath", `String "config/build.json") ])
           "echo nested"));
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
            Kiln_io.Driver.plan (request root [ ("development", `Bool true) ])
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
