module Planner = Kiln_planning.Planner
module Error = Kiln_core.Error
module Build_plan = Kiln_planning.Build_plan
module Provider = Kiln_planning.Provider
module Pins = Kiln_providers.Pins
open Contract_support

let providers : Provider.t list = [ (module Kiln_providers.Node) ]
let base = {|{"main":"main.js"}|}
let snapshot = snapshot [ ("package.json", base) ]
let run root extras = Kiln_io.Driver.plan ~providers (request root extras)
let result root extras = ok (prepare ~providers root extras)
let release pin = pin.Pins.node.Pins.release

let default_pin =
  last (List.filter (fun pin -> Option.is_some pin.Pins.lts) Pins.node)

let oldest_pin = List.hd Pins.node
let major version = List.hd (String.split_on_char '.' version)

let selected field response =
  Yojson.Basic.Util.(
    response.Planner.resolution |> member "data" |> member field |> to_string)

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
  snapshot "default npm plan" (fun root ->
      let plan = result root [] in
      check "default LTS" (uses_node (release default_pin) plan);
      check "bundled npm"
        (selected "packageManager" plan = "npm"
        && selected "packageManagerVersion" plan = default_pin.Pins.npm);
      let open Yojson.Basic.Util in
      List.iter
        (fun (name, source) ->
          let package = package plan name in
          check
            (name ^ " default provenance")
            (member "name" package = `String name
            && member "requestedVersion" package = `Null
            && member "source" package = `String source))
        [ ("node", "kiln default"); ("npm", "bundled with Node") ];
      let dockerfile = Build_plan.dockerfile plan.Planner.plan in
      let position text =
        match String.find_first ~sub:text dockerfile with
        | Some index -> index
        | None -> failwith ("missing: " ^ text)
      in
      check "dependencies install on the build platform for the target"
        (position "FROM --platform=$BUILDPLATFORM"
        < position {|--cpu=\"$cpu\" --libc=glibc --ignore-scripts|});
      check "the emulated fallback and the source copy follow"
        (position {|--ignore-scripts|} < position {|"kiln","npm","install"|}
        && position {|"kiln","npm","install"|} < position {|COPY --exclude|});
      check "the plan names the install path"
        (`Assoc (Planner.info plan)
        |> member "metadata" |> member "dependencyInstall"
        = `String "native, emulated if a package has an install script");
      check "main startup" (startup plan = Some [ "node"; "main.js" ]));
  List.iter
    (fun (label, pkg) ->
      snapshot label (fun root ->
          write root "package.json" pkg;
          let plan = result root [] in
          check "installs on the target platform"
            (not
               (contains
                  (Build_plan.dockerfile plan.Planner.plan)
                  "$BUILDPLATFORM"));
          check "the plan names the install path"
            Yojson.Basic.Util.(
              `Assoc (Planner.info plan)
              |> member "metadata" |> member "dependencyInstall"
              = `String "emulated")))
    [
      ( "root install script stays under emulation",
        {|{"main":"main.js","scripts":{"postinstall":"true"}}|} );
      ( "npm without --libc stays under emulation",
        {|{"main":"main.js","packageManager":"npm@10.1.0"}|} );
      ( "pnpm stays under emulation",
        {|{"main":"main.js","packageManager":"pnpm@11.0.0"}|} );
    ];
  snapshot "a major selects its pin and images" (fun root ->
      write root ".nvmrc" (major (release oldest_pin));
      let plan = result root [] in
      check "pin" (uses_node (release oldest_pin) plan);
      check "images"
        (selected "builderImage" plan = oldest_pin.Pins.node.Pins.builder
        && selected "runtimeImage" plan = oldest_pin.Pins.node.Pins.runtime));
  (let npm = (List.hd Pins.npm).Pins.tool in
   snapshot "devEngines select the runtime and a pinned manager" (fun root ->
       write root "package.json"
         (Printf.sprintf
            {|{"main":"main.js","devEngines":{"runtime":{"name":"node","version":"^%s"},"packageManager":[{"name":"npm","version":"%s"}]}}|}
            (major (release oldest_pin))
            (major npm));
       let plan = result root [] in
       check "runtime" (uses_node (release oldest_pin) plan);
       check "manager"
         (selected "packageManager" plan = "npm"
         && selected "packageManagerVersion" plan = npm)));

  (* Shared request and package validation. *)
  snapshot "version declarations must intersect" (fun root ->
      write root "package.json"
        (Printf.sprintf {|{"main":"main.js","engines":{"node":"^%s"}}|}
           (major (release oldest_pin)));
      write root ".node-version" (major (release (last Pins.node)));
      failure "no compatible Node" (run root []));
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
      let plan = result root [] in
      check "a workspace selects pnpm" (selected "packageManager" plan = "pnpm");
      check "build script executed" (runs "pnpm run build" plan);
      check "manager startup" (startup plan = Some [ "pnpm"; "run"; "start" ]);
      write root "pnpm-lock.yaml" "lockfileVersion: '9.0'\n";
      check "lockfile freezes installation"
        (runs "pnpm install --frozen-lockfile" (result root [])));
  snapshot "unsupported manager range" (fun root ->
      write root "package.json" {|{"main":"main.js","packageManager":"pnpm@1"}|};
      check "the failure lists the pinned versions"
        (lists_supported (run root [])
           (List.map (fun tool -> tool.Pins.tool) Pins.pnpm)));
  List.iter
    (fun (manager, version) ->
      let node = release default_pin in
      let package_json suffix =
        Printf.sprintf
          {|{"main":"main.js","engines":{"node":"%s"},"packageManager":"%s@%s%s"}|}
          node manager version suffix
      in
      snapshot
        ("exact " ^ manager ^ "@" ^ version)
        (fun root ->
          write root "package.json" (package_json "");
          let plan = result root [] in
          check "the declared version passes through"
            (selected "packageManagerVersion" plan = version);
          check "only a manager Node doesn't bundle is installed"
            (runs ("npm install --global " ^ manager ^ "@" ^ version) plan
            = (manager <> "npm"));
          let hex =
            "3ed639f09cd3ea741e752c5bd225635c916fee2c9452baf1e93deb17358edb72"
          in
          write root "package.json" (package_json ("+sha256." ^ hex));
          let dockerfile =
            Build_plan.dockerfile (result root []).Planner.plan
          in
          check "the build verifies an integrity-qualified manager"
            (contains dockerfile (hex ^ "  /tmp/kiln-manager.tgz")
            && contains dockerfile "sha256sum -c")))
    [ ("npm", default_pin.Pins.npm); ("pnpm", "12.12.1") ];
  snapshot "cancellation" (fun root ->
      List.iter
        (fun checks ->
          let remaining = ref checks in
          let cancelled () =
            decr remaining;
            !remaining < 0
          in
          check "cancelled planning cannot succeed"
            (Kiln_io.Driver.plan ~providers ~cancelled (request root [])
            = Error Error.Cancelled))
        [ 0; 1 ]);
  finish ()
