module Build_plan = Kiln_planning.Build_plan
open Contract_support

let dockerfile root = Build_plan.dockerfile (ok (prepare root []))

let () =
  snapshot
    [
      ("package.json", {|{"main":"main.ts"}|});
      ("bun.lock", "");
      ("railpack.json", {|{"provider":"node"}|});
    ]
    "the public Node provider selects the Bun implementation"
    (fun root ->
      check "Bun planned" (contains (dockerfile root) "docker.io/oven/bun@"));
  snapshot
    [
      ( "package.json",
        {|{"packageManager":"bun@1","scripts":{"start":"bun main.ts"}}|} );
    ]
    "Bun start script"
    (fun root ->
      check "declared startup"
        (contains (dockerfile root) {|CMD ["bun","run","start"]|}));
  snapshot
    [
      ("package.json", {|{"packageManager":"npm@10","main":"main.js"}|});
      ("bun.lockb", "");
    ]
    "explicit runtimes are ambiguous"
    (fun root ->
      check "ambiguous detection" (Result.is_error (prepare root []));
      write root "railpack.json" {|{"provider":"bun"}|};
      check "a configured provider still validates declarations"
        (Result.is_error (prepare root [])));
  snapshot
    [
      ( "package.json",
        {|{"packageManager":"bun@1","workspaces":["../outside"],"main":"main.ts"}|}
      );
    ]
    "Bun workspaces are contained"
    (fun root -> check "escape rejected" (Result.is_error (prepare root [])));
  finish ()
