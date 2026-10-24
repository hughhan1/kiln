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

(* Build steps as text: context copies and RUN argv. *)
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
    | Build_plan.Image _ -> [])
  @ own_steps stage

let steps prepared =
  inherited_steps
    (List.find
       (fun stage -> List.mem "source" (own_steps stage))
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

let workspace root =
  Unix.mkdir (Filename.concat root "packages") 0o700;
  Unix.mkdir (Filename.concat root "packages/local") 0o700;
  List.iter
    (fun (path, bytes) -> write root path bytes)
    [
      ("pnpm-workspace.yaml", "packages:\n  - packages/*\n");
      ("packages/local/package.json", {|{"name":"local","version":"1.0.0"}|});
      ( "package.json",
        {|{"scripts":{"start":"node main.js","build":"node build.js"},"dependencies":{"local":"workspace:*"}}|}
      );
    ]

let () =
  (* Planning output and resolution reuse. *)
  snapshot "default npm plan" (fun root ->
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
      check "dependencies installed" (runs "npm install" plan);
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
      workspace root;
      let platforms = ("platforms", `List [ `String "linux/arm64" ]) in
      let plan = result root [ platforms ] in
      check "compatible nondeprecated pnpm"
        (selected "packageManager" plan = "pnpm"
        && selected "packageManagerVersion" plan = "10.1.0");
      check "build script executed" (runs "pnpm run build" plan);
      check "manager startup" (startup plan = Some [ "pnpm"; "run"; "start" ]);
      write root "pnpm-lock.yaml" "lockfileVersion: '9.0'\n";
      let frozen =
        result root
          ~discovery:(fun _ _ -> failwith "lockfile invalidated toolchain")
          [ platforms; ("resolution", resolved plan) ]
      in
      check "lockfile freezes installation"
        (runs "pnpm install --frozen-lockfile" frozen);
      check "workspace manifests install before the source copy"
        (ordered (steps frozen)
           [
             "manifest package.json pnpm-lock.yaml pnpm-workspace.yaml";
             "manifest packages/local/package.json";
             "pnpm install";
             "source";
             "pnpm run build";
           ]));
  (* Local packages inside the source install with the manifests unless they
     need the rest of the source or belong to a workspace project. *)
  let local_package ?(script = "") ?(workspace = "") root reference =
    Unix.mkdir (Filename.concat root "packages") 0o700;
    Unix.mkdir (Filename.concat root "packages/local") 0o700;
    write root "packages/local/index.js" "";
    write root "packages/local/package.json"
      (Printf.sprintf {|{"name":"local","scripts":{%s}}|} script);
    write root "package.json"
      (Printf.sprintf {|{"main":"main.js",%s"dependencies":{"local":"%s"}}|}
         workspace reference)
  in
  List.iter
    (fun (label, setup, expected) ->
      snapshot label (fun root ->
          setup root;
          check "local package inputs"
            (ordered (steps (result root [])) expected)))
    [
      ( "local package installs before the source copy",
        (fun root -> local_package root "file:./packages/local"),
        [
          "manifest package.json";
          "manifest packages/local/index.js packages/local/package.json";
          "npm install";
          "source";
        ] );
      ( "normalized linked package",
        (fun root -> local_package root "link:./packages/../packages/local"),
        [
          "manifest packages/local/index.js packages/local/package.json";
          "npm install";
          "source";
        ] );
      ( "local package that builds from the source installs from it",
        (fun root ->
          local_package ~script:{|"prepare":"node ../../build.js"|} root
            "file:packages/local"),
        [ "source"; "npm install" ] );
      ( "source root package installs from the source",
        (fun root -> local_package root "file:."),
        [ "source"; "npm install" ] );
      ( "npm workspace keeps local packages in the source",
        (fun root ->
          local_package ~workspace:{|"workspaces":["packages/*"],|} root
            "file:packages/local"),
        [ "source"; "npm install" ] );
      ( "pnpm workspace keeps local packages in the source",
        (fun root ->
          local_package root "file:packages/local";
          write root "pnpm-workspace.yaml" "packages:\n  - packages/*\n"),
        [ "source"; "pnpm install" ] );
    ];
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
