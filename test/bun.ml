module Planner = Kiln_planning.Planner
module Build_plan = Kiln_planning.Build_plan
module Pins = Kiln_providers.Pins
open Contract_support

let () =
  snapshot
    [ ("package.json", {|{"main":"main.ts"}|}); ("bun.lock", "") ]
    "Bun lock selects Bun over Node fallback"
    (fun root ->
      let first = ok (prepare root []) in
      check "selected Bun"
        (Yojson.Basic.Util.member "provider" first.Planner.resolution
        = `String "bun");
      let selected = ok (prepare root [ ("provider", `String "node") ]) in
      check "public Node provider selects the Bun implementation"
        (selected.Planner.resolution = first.Planner.resolution
        && Yojson.Basic.Util.member "detectedProviders"
             (`Assoc (Planner.info selected))
           = `List [ `String "node" ]));
  let oldest = (List.hd Pins.bun).Pins.version in
  snapshot
    [
      ( "package.json",
        Printf.sprintf
          {|{"packageManager":"bun@%s","engines":{"bun":">=1"},"scripts":{"start":"bun main.ts"}}|}
          oldest );
    ]
    "Bun intersects declarations"
    (fun root ->
      let plan = ok (prepare root []) in
      check "declared startup"
        (Option.map Build_plan.arguments (Build_plan.startup plan.Planner.plan)
        = Some [ "bun"; "run"; "start" ]);
      write root "package.json"
        {|{"engines":{"bun":">=1"},"scripts":{"start":"bun main.ts"}}|};
      write root ".bun-version" oldest;
      let open Yojson.Basic.Util in
      let package = package (ok (prepare root [])) "bun" in
      check "the plan reports the declaration origins"
        (member "resolvedVersion" package = `String oldest
        && member "requestedVersion" package = `String (oldest ^ " && >=1")
        && member "source" package
           = `String ".bun-version, package.json engines.bun"));
  snapshot
    [
      ("package.json", {|{"packageManager":"npm@10","main":"main.js"}|});
      ("bun.lockb", "");
    ]
    "explicit runtimes are ambiguous"
    (fun root ->
      check "ambiguous detection" (Result.is_error (prepare root []));
      check "override still validates declarations"
        (Result.is_error (prepare root [ ("provider", `String "bun") ])));
  snapshot
    [
      ( "package.json",
        {|{"packageManager":"bun@1","workspaces":["../outside"],"main":"main.ts"}|}
      );
    ]
    "Bun workspaces are contained"
    (fun root -> check "escape rejected" (Result.is_error (prepare root [])));
  finish ()
