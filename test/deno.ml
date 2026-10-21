module Planner = Kiln_planning.Planner
module Build_plan = Kiln_planning.Build_plan
open Contract_support

let fetch =
  registry
    [
      ("denoland/deno", [ "1.46.3"; "2.4.3"; "2.5.1"; "2.6.0-rc.1"; "alpine" ]);
    ]

let prepare ?(fetch = fetch) root fields =
  Kiln_io.Driver.prepare_with ~today:"2026-10-12" ~fetch (request root fields)

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
      check "stable Deno"
        (Yojson.Basic.Util.(
           first.Planner.resolution |> member "data" |> member "version")
        = `String "2.5.1");
      write root "deno.jsonc" {|{"tasks":{"start":"deno run other.ts"}}|};
      let reused =
        ok
          (prepare
             ~fetch:(fun _ _ -> failwith "offline discovery")
             root
             [ ("resolution", first.Planner.resolution) ])
      in
      check "startup preserves resolution"
        (reused.Planner.resolution = first.Planner.resolution));
  snapshot
    [
      ("deno.json", {|{"tasks":{"start":"deno run main.ts"}}|});
      (".deno-version", "2.4.3");
    ]
    "Deno exact version avoids tag discovery"
    (fun root ->
      ignore
        (ok
           (prepare
              ~fetch:(fun url token ->
                check "no catalog"
                  (not (String.ends_with ~suffix:"tags/list" url));
                fetch url token)
              root [])));
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
