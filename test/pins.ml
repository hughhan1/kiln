module Pins = Kiln_providers.Pins
open Contract_support

let images pins = List.map (fun pin -> pin.Pins.version) pins
let nodes = List.map (fun pin -> pin.Pins.node.Pins.release) Pins.node

let default_node =
  last
    (List.filter_map
       (fun pin ->
         Option.map (fun _ -> pin.Pins.node.Pins.release) pin.Pins.lts)
       Pins.node)

let () =
  List.iter
    (fun (name, files, pins, default) ->
      snapshot files (name ^ " selects from its pins") (fun root ->
          let resolved fields =
            Yojson.Basic.Util.member "resolvedVersion"
              (package (ok (prepare root fields)) name)
          in
          let requesting version =
            [
              ( "environment",
                `Assoc
                  [
                    ( "RAILPACK_" ^ String.uppercase_ascii name ^ "_VERSION",
                      `String version );
                  ] );
            ]
          in
          check "default" (resolved [] = `String default);
          List.iter
            (fun pin ->
              check
                ("every pin plans: " ^ pin)
                (resolved (requesting pin) = `String pin))
            pins;
          check "an unsupported version lists what is supported"
            (lists_supported (prepare root (requesting "0.0.1")) pins)))
    [
      ("node", [ ("package.json", "{}") ], nodes, default_node);
      ( "bun",
        [ ("package.json", "{}"); ("bun.lock", "") ],
        images Pins.bun,
        last (images Pins.bun) );
      ( "deno",
        [ ("deno.json", "{}") ],
        images Pins.deno,
        last (images Pins.deno) );
      ( "python",
        [ ("main.py", "") ],
        List.map (fun pin -> pin.Pins.release) Pins.python,
        (last Pins.python).Pins.release );
      ( "uv",
        [
          ("main.py", "");
          ("pyproject.toml", "[project]\nrequires-python='>=3.11'\n");
          ("uv.lock", "version=1\nrequires-python='>=3.11'\n");
        ],
        images Pins.uv,
        last (images Pins.uv) );
      ( "go",
        [
          ( "go.mod",
            "module example\ngo " ^ (List.hd Pins.golang).Pins.version ^ "\n" );
        ],
        images Pins.golang,
        (List.hd Pins.golang).Pins.version );
    ];
  finish ()
