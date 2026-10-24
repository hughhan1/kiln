module Planner = Kiln_planning.Planner
module Error = Kiln_core.Error
module Build_plan = Kiln_planning.Build_plan
module Provider = Kiln_planning.Provider
open Contract_support
open Node_fixture

let providers : Provider.t list = [ (module Kiln_providers.Node) ]
let base = {|{"main":"main.js"}|}
let snapshot = snapshot [ ("package.json", base) ]

let run root ?(discovery = fetch) ?(today = "2026-10-11") extras =
  Kiln_io.Driver.plan_with ~providers ~today ~fetch:discovery
    (request root extras)

let result root ?(discovery = fetch) ?(today = "2026-10-11") extras =
  ok
    (Kiln_io.Driver.prepare_with ~providers ~today ~fetch:discovery
       (request root extras))

let resolved prepared = prepared.Planner.resolution

let selected field response =
  Yojson.Basic.Util.(
    resolved response |> member "data" |> member field |> to_string)

let startup response =
  Option.map Build_plan.arguments (Build_plan.startup response.Planner.plan)

let runs command response =
  let argv =
    Kiln_planning.Json.string_array (String.split_on_char ' ' command)
  in
  let prefix = String.drop_last 1 (Kiln_planning.Json.encode argv) in
  contains (Build_plan.dockerfile response.Planner.plan) ("RUN " ^ prefix)

let uses_node version response = selected "nodeVersion" response = version

let failure expected = function
  | Ok response -> failwith ("expected failure: " ^ response)
  | Error error ->
      check (Error.show error) (contains (Error.show error) expected)

(* Context-copy inputs and RUN argv in execution order. *)
let steps stage =
  List.filter_map
    (function
      | Build_plan.Copy { input = Build_plan.Context; paths; _ } ->
          Some (String.concat " " paths)
      | Build_plan.Run argv ->
          Some (String.concat " " (Build_plan.arguments argv))
      | Build_plan.Copy
          { input = Build_plan.Output _ | Build_plan.External _; _ }
      | Build_plan.Workdir _ | Build_plan.Env _ | Build_plan.User _
      | Build_plan.Entrypoint _ | Build_plan.Start _ | Build_plan.Prepend_path _
      | Build_plan.Write _ ->
          None)
    (Build_plan.operations stage)

(* Inspect the stages the runtime actually copies, including their ancestry. *)
let layout prepared =
  let copies =
    List.filter_map
      (function
        | Build_plan.Copy { input = Build_plan.Output stage; exclude; paths; _ }
          ->
            Some ((exclude, paths), stage)
        | Build_plan.Copy
            { input = Build_plan.Context | Build_plan.External _; _ }
        | Build_plan.Run _ | Build_plan.Workdir _ | Build_plan.Env _
        | Build_plan.User _ | Build_plan.Entrypoint _ | Build_plan.Start _
        | Build_plan.Prepend_path _ | Build_plan.Write _ ->
            None)
      (Build_plan.operations prepared.Planner.plan)
  in
  let dependencies =
    List.assoc ([ "*"; "!**/node_modules" ], [ "/app/" ]) copies
  in
  let build = List.assoc ([ "**/node_modules" ], [ "/app/" ]) copies in
  check "build inherits installed dependencies"
    (dependencies == build
    || Build_plan.base build = Build_plan.Stage dependencies);
  check "dependency artifacts come from the dependency stage"
    (List.assoc ([], [ "/opt/app-deps/" ]) copies == dependencies);
  (steps dependencies, steps build)

let local_package ?(script = "") ?(workspace = "") root reference =
  Unix.mkdir (Filename.concat root "packages") 0o700;
  Unix.mkdir (Filename.concat root "packages/local") 0o700;
  write root "packages/local/index.js" "";
  write root "packages/local/package.json"
    (Printf.sprintf {|{"name":"local","version":"1.0.0","scripts":{%s}}|} script);
  write root "package.json"
    (Printf.sprintf
       {|{"main":"main.js","scripts":{"start":"node main.js","build":"node build.js"},%s"dependencies":{"local":"%s"}}|}
       workspace reference)

let () =
  (* Planning output and resolution reuse. *)
  snapshot "default npm plan" (fun root ->
      write root "package-lock.json" "{}";
      let plan =
        result root [ ("platforms", `List [ `String "linux/amd64" ]) ]
      in
      check "default LTS" (uses_node "24.0.0" plan);
      check "bundled npm"
        (selected "packageManager" plan = "npm"
        && selected "packageManagerVersion" plan = "11.0.0");
      let open Yojson.Basic.Util in
      let packages = `Assoc (Planner.info plan) |> member "resolvedPackages" in
      List.iter
        (fun (name, source) ->
          let package = member name packages in
          check
            (name ^ " default provenance")
            (member "name" package = `String name
            && member "requestedVersion" package = `Null
            && member "source" package = `String source))
        [ ("node", "kiln default"); ("npm", "bundled with Node") ];
      check
        "locked npm dependencies and artifacts precede the source in a \
         separate stage"
        (layout plan
        = ([ "package-lock.json package.json"; "npm install" ], [ "." ]));
      check "install and start commands see the artifact directory"
        (contains
           (Build_plan.dockerfile plan.Planner.plan)
           "RUN [\"mkdir\",\"-p\",\"/opt/app-deps\"]"
        && List.length
             (String.split_on_char '\n'
                (Build_plan.dockerfile plan.Planner.plan)
             |> List.filter (fun line ->
                 contains line "APP_DEPS_DIR=\"/opt/app-deps\""))
           = 2);
      check "main startup" (startup plan = Some [ "node"; "main.js" ]));
  snapshot "reuse and invalidation" (fun root ->
      let initial = result root [] in
      let reuse extras =
        result root
          ~discovery:(fun _ _ -> failwith "unexpected discovery")
          ~today:"2040-01-01"
          (("resolution", resolved initial) :: extras)
      in
      write root "main.js" "source edit";
      check "source edits preserve the plan after EOL" (reuse [] = initial);
      check "platforms are a set"
        (reuse
           [
             ( "platforms",
               `List
                 [
                   `String "linux/arm64";
                   `String "linux/amd64";
                   `String "linux/amd64";
                 ] );
           ]
        = initial);
      write root "package.json"
        {|{"main":"main.js","engines":{"node":"^22.20"}}|};
      check "requirements invalidate"
        (uses_node "22.20.0" (result root [ ("resolution", resolved initial) ])));
  List.iter
    (fun (pkg, node, manager, version) ->
      snapshot
        (manager ^ "@" ^ version)
        (fun root ->
          write root "package.json" pkg;
          let plan = result root [] in
          check "runtime selection" (uses_node node plan);
          check "manager selection"
            (selected "packageManager" plan = manager
            && selected "packageManagerVersion" plan = version)))
    [
      ( {|{"main":"main.js","devEngines":{"runtime":{"name":"node","version":"^22"},"packageManager":[{"name":"npm","version":"~10.1"}]}}|},
        "22.20.0",
        "npm",
        "10.1.0" );
      ( {|{"main":"main.js","packageManager":"pnpm@11.0.0"}|},
        "24.0.0",
        "pnpm",
        "11.0.0" );
    ];

  (* Shared request and package validation. *)
  List.iter
    (fun (label, files, message) ->
      snapshot label (fun root ->
          List.iter (fun (path, bytes) -> write root path bytes) files;
          failure message (run root [])))
    [
      ( "engine/file conflict",
        [
          ("package.json", {|{"main":"main.js","engines":{"node":"^22.20"}}|});
          (".node-version", "24");
        ],
        "no supported stable Node" );
      ( "version-file conflict",
        [ (".node-version", "22.20.0\n"); (".nvmrc", "24.0.0\n") ],
        "no supported stable Node" );
      ("EOL runtime", [ (".node-version", "20.19.0") ], "EOL");
    ];
  List.iter
    (fun (pkg, message) ->
      snapshot ("package: " ^ message) (fun root ->
          write root "package.json" pkg;
          failure message (run root [])))
    [
      ({|{"main":"main.js","packageManager":"yarn@4.0.0"}|}, "package manager");
      ( {|{"main":"main.js","packageManager":"npm@11","engines":{"pnpm":"10"}}|},
        "conflicting" );
      ({|{"main":"main.js","engines":{"node":"oops"}}|}, "version range");
      ( {|{"main":"main.js","workspaces":["../outside"]}|},
        "workspace pattern escapes source: ../outside" );
      ( {|{"main":"main.js","dependencies":{"local":"file:./missing"}}|},
        "filesystem" );
      ({|{"devEngines":{"runtime":{"name":"bun"}}}|}, "only Node");
      ( {|{"main":"main.js","packageManager":"pnpm@10+sha256.bad"}|},
        "hex digest" );
      ({|{"main":"main.js","scripts":{"build":42}}|}, "expected string");
      ("null", "expected object");
      ("{broken", "package.json");
    ];
  (* Source filtering and containment. *)
  snapshot "development exclusions" (fun root ->
      let initial = result root [] in
      List.iter
        (fun dir ->
          Unix.mkdir (Filename.concat root dir) 0o700;
          write root (dir ^ "/package.json") "invalid ignored metadata")
        [ ".git"; "node_modules" ];
      let project = result root [] in
      check "development directories do not change the plan" (project = initial);
      write root "package.json"
        {|{"main":"main.js","dependencies":{"local":"file:node_modules"}}|};
      failure "escapes" (run root []);
      write root "package.json" base;
      Unix.symlink "node_modules/package.json"
        (Filename.concat root "linked.json");
      failure "escapes" (run root []));
  List.iter
    (fun (setup, message) ->
      snapshot message (fun root ->
          setup (Filename.concat root "entry");
          failure message (run root [])))
    [
      ((fun path -> Unix.symlink "/tmp" path), "escapes");
      ((fun path -> Unix.mkfifo path 0o600), "non-regular");
    ];

  (* Workspace configuration, startup, and lockfile handling. *)
  snapshot "workspace manifests and lockfile" (fun root ->
      local_package root "workspace:*";
      write root "pnpm-workspace.yaml" "packages:\n  - packages/*\n";
      let platforms = ("platforms", `List [ `String "linux/arm64" ]) in
      let plan = result root [ platforms ] in
      check "compatible nondeprecated pnpm"
        (selected "packageManager" plan = "pnpm"
        && selected "packageManagerVersion" plan = "10.1.0");
      check "manager startup" (startup plan = Some [ "pnpm"; "run"; "start" ]);
      write root "pnpm-lock.yaml" "lockfileVersion: '9.0'\n";
      let frozen =
        result root
          ~discovery:(fun _ _ -> failwith "lockfile invalidated toolchain")
          [ platforms; ("resolution", resolved plan) ]
      in
      check
        "workspace dependencies precede source and build in a separate stage"
        (layout frozen
        = ( [
              "package.json pnpm-lock.yaml pnpm-workspace.yaml";
              "packages/local/package.json";
              "pnpm install --frozen-lockfile --prefer-offline";
            ],
            [ "."; "pnpm run build" ] )));
  (* Local package contents are install inputs; lifecycle scripts and local
     workspace references still require the whole source. *)
  let manifests =
    ( [
        "package.json";
        "packages/local/index.js packages/local/package.json";
        "npm install";
      ],
      [ "."; "npm run build" ] )
  in
  let whole = [ "."; "npm install"; "npm run build" ] in
  let pnpm_whole = [ "."; "pnpm install"; "pnpm run build" ] in
  List.iter
    (fun (label, setup, expected) ->
      snapshot label (fun root ->
          setup root;
          check "runtime dependency and build inputs"
            (layout (result root []) = expected)))
    [
      ( "local package installs before the source copy",
        (fun root -> local_package root "file:./packages/local"),
        manifests );
      ( "normalized linked package",
        (fun root -> local_package root "link:./packages/../packages/local"),
        manifests );
      ( "local package that builds from the source installs from it",
        (fun root ->
          local_package ~script:{|"prepare":"node ../../build.js"|} root
            "file:packages/local"),
        (whole, whole) );
      ( "root lifecycle script installs from the source",
        (fun root ->
          write root "package.json"
            {|{"main":"main.js","scripts":{"build":"node build.js","postinstall":"node setup.js"}}|}),
        (whole, whole) );
      ( "source root package installs from the source",
        (fun root -> local_package root "file:."),
        (whole, whole) );
      ( "npm workspace keeps local packages in the source",
        (fun root ->
          local_package ~workspace:{|"workspaces":["packages/*"],|} root
            "file:packages/local"),
        (whole, whole) );
      ( "pnpm workspace keeps local packages in the source",
        (fun root ->
          local_package root "file:packages/local";
          write root "pnpm-workspace.yaml" "packages:\n  - packages/*\n"),
        (pnpm_whole, pnpm_whole) );
    ];
  snapshot "install commands surround inherited installation" (fun root ->
      write root "railpack.json"
        {|{"steps":{"install":{"commands":["node -e 1","...","node -e 2"]}}}|};
      check "configured commands stay in the dependency stage, in order"
        (layout (result root [])
        = ( [
              "/bin/sh -c node -e 1";
              "package.json";
              "npm install";
              "/bin/sh -c node -e 2";
            ],
            [ "." ] )));
  List.iter
    (fun (manager, version) ->
      let node = "24.21.0" in
      snapshot
        ("upstream " ^ manager ^ "@" ^ version)
        (fun root ->
          write root "package.json"
            (Printf.sprintf
               {|{"main":"main.js","engines":{"node":"%s"},"packageManager":"%s@%s"}|}
               node manager version);
          let responses =
            [
              ( "https://nodejs.org/dist/index.json",
                Printf.sprintf
                  {|[{"version":"v%s","npm":"11.19.0","lts":"Krypton"}]|} node
              );
              ( "https://registry.npmjs.org/" ^ manager,
                Printf.sprintf
                  {|{"versions":{"%s":{"engines":{"node":">=24"}}}}|} version );
            ]
          in
          let discovery = discovery responses in
          let plan = result root ~discovery [] in
          List.iter
            (fun field ->
              check ("upstream " ^ field)
                (String.starts_with ~prefix:"docker.io/library/node@sha256:"
                   (selected field plan)))
            [ "builderImage"; "runtimeImage" ];
          check "package manager setup"
            (runs ("npm install --global " ^ manager ^ "@" ^ version) plan
            = (manager <> "npm"));
          let hex =
            "3ed639f09cd3ea741e752c5bd225635c916fee2c9452baf1e93deb17358edb72"
          in
          write root "package.json"
            (Printf.sprintf
               {|{"main":"main.js","engines":{"node":"%s"},"packageManager":"%s@%s+sha256.%s"}|}
               node manager version hex);
          let tarball =
            "https://registry.npmjs.org/" ^ manager ^ "/-/" ^ manager ^ "-"
            ^ version ^ ".tgz"
          in
          let verified =
            result root
              ~discovery:
                (Node_fixture.discovery
                   ((tarball, "package-manager-tarball") :: responses))
              []
          in
          check "integrity-qualified manager is installed from verified bytes"
            (contains
               (Build_plan.dockerfile verified.Planner.plan)
               (hex ^ "  /tmp/kiln-manager.tgz"));
          failure "integrity does not match"
            (run root
               ~discovery:
                 (Node_fixture.discovery ((tarball, "corrupt") :: responses))
               [])))
    [ ("npm", "11.19.0"); ("pnpm", "12.12.1") ];
  (* Discovery verifies upstream data and honors cancellation. *)
  List.iter
    (fun (responses, message) ->
      snapshot message (fun root ->
          failure message (run root ~discovery:(discovery responses) [])))
    [
      ( List.map
          (fun (url, _) ->
            ( url,
              {|{"manifests":[{"platform":{"os":"linux","architecture":"arm64"}}]}|}
            ))
          image_urls,
        "lacks a requested target" );
      ( [
          ( "https://nodejs.org/dist/index.json",
            {|[{"version":"v24.0.0","lts":"Krypton"}]|} );
        ],
        "missing its bundled npm" );
    ];
  List.iter
    (fun before ->
      snapshot
        ("cancellation before discovery: " ^ string_of_bool before)
        (fun root ->
          let cancelled = ref before in
          let fetch url token =
            let response = fetch url token in
            cancelled := true;
            response
          in
          check "cancelled planning cannot succeed"
            (Kiln_io.Driver.plan_with ~providers ~today:"2026-10-11"
               ~cancelled:(fun () -> !cancelled)
               ~fetch (request root [])
            = Error Error.Cancelled)))
    [ true; false ];
  finish ()
