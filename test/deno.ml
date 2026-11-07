module Planner = Kiln_planning.Planner
module Build_plan = Kiln_planning.Build_plan
module Pins = Kiln_providers.Pins
open Contract_support

let pins = List.map (fun pin -> pin.Pins.version) Pins.deno
let prepare root fields = Kiln_io.Driver.prepare (request root fields)

let version prepared =
  Yojson.Basic.Util.(
    prepared.Planner.resolution |> member "data" |> member "version")

let () =
  snapshot
    [
      ( "deno.jsonc",
        {|{
    // comments and trailing commas outside strings
    "tasks":{"start":"echo https://example.com/a,} /* text */ \\\"quoted\\\"",},
    "imports": {},
  }|}
      );
      ("package.json", "{}");
    ]
    "Deno configuration wins over package fallback"
    (fun root ->
      let first = ok (prepare root []) in
      check "Deno selected"
        (Yojson.Basic.Util.member "provider" first.Planner.resolution
        = `String "deno");
      check "Deno task startup"
        (Option.map Build_plan.arguments (Build_plan.startup first.Planner.plan)
        = Some [ "deno"; "task"; "start" ]);
      check "newest pin" (version first = `String (last pins)));
  snapshot
    [
      ("deno.json", {|{"tasks":{"start":"deno run main.ts"}}|});
      (".deno-version", List.hd pins);
    ]
    "Deno version file selects its pin"
    (fun root ->
      check "selected pin"
        (version (ok (prepare root [])) = `String (List.hd pins));
      write root ".deno-version" "1.46.3";
      check "an unsupported version lists what is supported"
        (lists_supported (prepare root []) pins));
  List.iter
    (fun (label, bytes) ->
      snapshot
        [ ("deno.json", bytes); ("deno.lock", "{}") ]
        label
        (fun root ->
          check "invalid configuration rejected"
            (Result.is_error (prepare root []))))
    [
      ( "outside workspace",
        {|{"workspace":["../outside"],"tasks":{"start":"echo hello"}}|} );
      ( "disabled existing lock",
        {|{"lock":false,"tasks":{"start":"echo hello"}}|} );
      ( "unsupported import map",
        {|{"importMap":"elsewhere.json","tasks":{"start":"echo hello"}}|} );
    ];
  finish ()
