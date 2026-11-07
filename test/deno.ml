module Planner = Kiln_planning.Planner
module Build_plan = Kiln_planning.Build_plan
module Pins = Kiln_providers.Pins
open Contract_support

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
      let plan = ok (prepare root []) in
      check "Deno selected"
        (Yojson.Basic.Util.member "provider" plan.Planner.resolution
        = `String "deno");
      check "Deno task startup"
        (Option.map Build_plan.arguments (Build_plan.startup plan.Planner.plan)
        = Some [ "deno"; "task"; "start" ]));
  let oldest = (List.hd Pins.deno).Pins.version in
  snapshot
    [
      ("deno.json", {|{"tasks":{"start":"deno run main.ts"}}|});
      (".deno-version", oldest);
    ]
    "Deno version file selects its pin"
    (fun root ->
      check "selected pin"
        (Yojson.Basic.Util.member "resolvedVersion"
           (package (ok (prepare root [])) "deno")
        = `String oldest));
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
