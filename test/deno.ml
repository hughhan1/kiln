module Build_plan = Kiln_planning.Build_plan
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
      check "Deno task startup"
        (contains
           (Build_plan.dockerfile (ok (prepare root [])))
           {|CMD ["deno","task","start"]|}));
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
