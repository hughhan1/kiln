module Error = Kiln_planning.Error
module Build_plan = Kiln_planning.Build_plan
module Provider = Kiln_planning.Provider
module Pins = Kiln_providers.Pins
open Contract_support

let providers : Provider.t list = [ (module Kiln_providers.Node) ]
let base = {|{"main":"main.js"}|}
let snapshot = snapshot [ ("package.json", base) ]
let run root extras = Kiln_io.Driver.plan ~providers (request root extras)

let result root extras =
  Build_plan.dockerfile (ok (prepare ~providers root extras))

let release pin = pin.Pins.node.Pins.release

let default_pin =
  last (List.filter (fun pin -> Option.is_some pin.Pins.lts) Pins.node)

let oldest_pin = List.hd Pins.node
let major version = List.hd (String.split_on_char '.' version)

(* What a declaration of the oldest pinned major resolves to. *)
let oldest_major =
  let line = major (release oldest_pin) in
  last (List.filter (fun pin -> major (release pin) = line) Pins.node)

let images pin dockerfile =
  contains dockerfile pin.Pins.node.Pins.builder
  && contains dockerfile pin.Pins.node.Pins.runtime

let runs command dockerfile =
  let argv =
    Kiln_planning.Json.string_array (String.split_on_char ' ' command)
  in
  let prefix = String.drop_last 1 (Kiln_planning.Json.encode argv) in
  contains dockerfile ("RUN " ^ prefix)

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
      let dockerfile = result root [] in
      let position text =
        match String.find_first ~sub:text dockerfile with
        | Some index -> index
        | None -> failwith ("missing: " ^ text)
      in
      check "dependencies install on the build platform for the target"
        (position "FROM --platform=$BUILDPLATFORM"
        < position {|--cpu=\"$cpu\" --libc=glibc --ignore-scripts|});
      check "the emulated fallback and the source copy follow"
        (position {|--ignore-scripts|} < position {|"npm","install"]|}
        && position {|"npm","install"]|} < position {|COPY [".","."]|});
      check "main startup" (contains dockerfile {|CMD ["node","main.js"]|}));
  List.iter
    (fun (label, pkg) ->
      snapshot label (fun root ->
          write root "package.json" pkg;
          check "installs on the target platform"
            (not (contains (result root []) "$BUILDPLATFORM"))))
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
      check "images" (images oldest_major (result root [])));
  (let npm = (List.hd Pins.npm).Pins.tool in
   snapshot "devEngines select the runtime and a pinned manager" (fun root ->
       write root "package.json"
         (Printf.sprintf
            {|{"main":"main.js","devEngines":{"runtime":{"name":"node","version":"^%s"},"packageManager":[{"name":"npm","version":"%s"}]}}|}
            (major (release oldest_pin))
            (major npm));
       let dockerfile = result root [] in
       check "runtime" (images oldest_major dockerfile);
       check "manager" (runs ("npm install --global npm@" ^ npm) dockerfile)));

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
  (* Source containment. *)
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
      let dockerfile = result root [] in
      check "build script executed" (runs "pnpm run build" dockerfile);
      check "manager startup"
        (contains dockerfile {|CMD ["pnpm","run","start"]|});
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
          check "only a manager Node doesn't bundle is installed"
            (runs
               ("npm install --global " ^ manager ^ "@" ^ version)
               (result root [])
            = (manager <> "npm"));
          let hex =
            "3ed639f09cd3ea741e752c5bd225635c916fee2c9452baf1e93deb17358edb72"
          in
          write root "package.json" (package_json ("+sha256." ^ hex));
          let dockerfile = result root [] in
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
