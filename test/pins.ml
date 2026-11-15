module Build_plan = Kiln_planning.Build_plan
module Pins = Kiln_providers.Pins
open Contract_support

let images pins = List.map (fun pin -> (pin.Pins.version, pin.Pins.image)) pins

let toolchains pins =
  List.map (fun pin -> (pin.Pins.release, pin.Pins.runtime)) pins

let nodes = toolchains (List.map (fun pin -> pin.Pins.node) Pins.node)
let oldest_go = List.hd Pins.golang

let default_node =
  (last (List.filter (fun pin -> Option.is_some pin.Pins.lts) Pins.node))
    .Pins.node
    .Pins.runtime

let () =
  List.iter
    (fun (name, files, request, pins, default) ->
      snapshot files (name ^ " selects from its pins") (fun root ->
          let requesting version =
            List.iter
              (fun (file, bytes) -> write root file bytes)
              (request version);
            prepare root []
          in
          let selects image prepared =
            contains (Build_plan.dockerfile (ok prepared)) image
          in
          check "default" (selects default (prepare root []));
          List.iter
            (fun (version, image) ->
              check
                ("every pin plans: " ^ version)
                (selects image (requesting version)))
            pins;
          check "an unsupported version lists what is supported"
            (lists_supported (requesting "0.0.1") (List.map fst pins))))
    [
      ( "node",
        [ ("package.json", "{}") ],
        (fun version -> [ (".node-version", version) ]),
        nodes,
        default_node );
      ( "bun",
        [ ("package.json", "{}"); ("bun.lock", "") ],
        (fun version -> [ (".bun-version", version) ]),
        images Pins.bun,
        (last Pins.bun).Pins.image );
      ( "deno",
        [ ("deno.json", "{}") ],
        (fun version -> [ (".deno-version", version) ]),
        images Pins.deno,
        (last Pins.deno).Pins.image );
      ( "python",
        [ ("main.py", "") ],
        (fun version -> [ (".python-version", version) ]),
        toolchains Pins.python,
        (last Pins.python).Pins.runtime );
      ( "uv",
        [
          ("main.py", "");
          ("pyproject.toml", "[project]\nrequires-python='>=3.11'\n");
          ("uv.lock", "version=1\nrequires-python='>=3.11'\n");
        ],
        (fun version ->
          [ ("uv.toml", "required-version = '==" ^ version ^ "'\n") ]),
        images Pins.uv,
        (last Pins.uv).Pins.image );
      ( "go",
        [
          ("go.mod", "module example\ngo " ^ oldest_go.Pins.version ^ "\n");
          ("main.go", "package main\n");
        ],
        (fun version ->
          [
            ( "go.mod",
              "module example\ngo " ^ version ^ "\ntoolchain go" ^ version
              ^ "\n" );
          ]),
        images Pins.golang,
        oldest_go.Pins.image );
    ];
  finish ()
