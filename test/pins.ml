module Planner = Kiln_planning.Planner
module Pins = Kiln_providers.Pins
open Contract_support

let images pins = List.map (fun pin -> pin.Pins.version) pins
let nodes = List.map (fun pin -> pin.Pins.node.Pins.release) Pins.node
let oldest_go = (List.hd Pins.golang).Pins.version

let default_node =
  last
    (List.filter_map
       (fun pin ->
         Option.map (fun _ -> pin.Pins.node.Pins.release) pin.Pins.lts)
       Pins.node)

let () =
  List.iter
    (fun (name, files, request, path, pins, default) ->
      snapshot files (name ^ " selects from its pins") (fun root ->
          let requesting version =
            List.iter
              (fun (file, bytes) -> write root file bytes)
              (request version);
            prepare root []
          in
          let resolved prepared =
            List.fold_left
              (fun json key -> Yojson.Basic.Util.member key json)
              (ok prepared).Planner.resolution ("data" :: path)
          in
          check "default" (resolved (prepare root []) = `String default);
          List.iter
            (fun pin ->
              check
                ("every pin plans: " ^ pin)
                (resolved (requesting pin) = `String pin))
            pins;
          check "an unsupported version lists what is supported"
            (lists_supported (requesting "0.0.1") pins)))
    [
      ( "node",
        [ ("package.json", "{}") ],
        (fun version -> [ (".node-version", version) ]),
        [ "nodeVersion" ],
        nodes,
        default_node );
      ( "bun",
        [ ("package.json", "{}"); ("bun.lock", "") ],
        (fun version -> [ (".bun-version", version) ]),
        [ "version" ],
        images Pins.bun,
        last (images Pins.bun) );
      ( "deno",
        [ ("deno.json", "{}") ],
        (fun version -> [ (".deno-version", version) ]),
        [ "version" ],
        images Pins.deno,
        last (images Pins.deno) );
      ( "python",
        [ ("main.py", "") ],
        (fun version -> [ (".python-version", version) ]),
        [ "python" ],
        List.map (fun pin -> pin.Pins.release) Pins.python,
        (last Pins.python).Pins.release );
      ( "uv",
        [
          ("main.py", "");
          ("pyproject.toml", "[project]\nrequires-python='>=3.11'\n");
          ("uv.lock", "version=1\nrequires-python='>=3.11'\n");
        ],
        (fun version ->
          [ ("uv.toml", "required-version = '==" ^ version ^ "'\n") ]),
        [ "installer"; "version" ],
        images Pins.uv,
        last (images Pins.uv) );
      ( "go",
        [ ("go.mod", "module example\ngo " ^ oldest_go ^ "\n") ],
        (fun version ->
          [
            ( "go.mod",
              "module example\ngo " ^ version ^ "\ntoolchain go" ^ version
              ^ "\n" );
          ]),
        [ "compiler"; "version" ],
        images Pins.golang,
        oldest_go );
    ];
  finish ()
