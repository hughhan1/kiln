module Planner = Kiln_planning.Planner
module Build_plan = Kiln_planning.Build_plan
open Contract_support

let fetch =
  registry
    [ ("oven/bun", [ "1.2.9"; "1.3.1"; "1.3.2"; "1.4.0-canary.1"; "slim" ]) ]

let prepare ?(fetch = fetch) root fields =
  Kiln_io.Driver.prepare_with ~today:"2026-10-12" ~fetch (request root fields)

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
           = `List [ `String "node" ]);
      check "stable version"
        (Yojson.Basic.Util.(
           first.Planner.resolution |> member "data" |> member "version")
        = `String "1.3.2");
      write root "package.json" {|{"main":"other.ts"}|};
      let reused =
        ok
          (prepare
             ~fetch:(fun _ _ -> failwith "offline discovery")
             root
             [ ("resolution", first.Planner.resolution) ])
      in
      check "startup edits retain toolchain"
        (first.Planner.resolution = reused.Planner.resolution));
  snapshot
    [
      ( "package.json",
        {|{"packageManager":"bun@1.3.1","engines":{"bun":">=1.3 <1.4"},"scripts":{"start":"bun main.ts"}}|}
      );
    ]
    "Bun intersects declarations without tag enumeration"
    (fun root ->
      let fetch url token =
        check "exact version skips tags"
          (not (String.ends_with ~suffix:"/tags/list" url));
        fetch url token
      in
      let plan = ok (prepare ~fetch root []) in
      check "declared startup"
        (Option.map Build_plan.arguments (Build_plan.startup plan.Planner.plan)
        = Some [ "bun"; "run"; "start" ]);
      write root "package.json"
        {|{"engines":{"bun":">=1.3 <1.4"},"scripts":{"start":"bun main.ts"}}|};
      write root ".bun-version" "1.3.1";
      let reused =
        ok
          (prepare
             ~fetch:(fun _ _ ->
               failwith "unchanged constraints performed discovery")
             root
             [ ("resolution", plan.Planner.resolution) ])
      in
      let open Yojson.Basic.Util in
      let package =
        `Assoc (Planner.info reused)
        |> member "resolvedPackages" |> member "bun"
      in
      check "reused versions report the current declaration origins"
        (member "requestedVersion" package = `String "1.3.1 && >=1.3 <1.4"
        && member "source" package
           = `String ".bun-version, package.json engines.bun");
      write root ".bun-version" "1.2.9";
      check "conflicting versions rejected" (Result.is_error (prepare root [])));
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
        {|{"packageManager":"bun@1.3.1","workspaces":["../outside"],"main":"main.ts"}|}
      );
    ]
    "Bun workspaces are contained"
    (fun root -> check "escape rejected" (Result.is_error (prepare root [])));
  finish ()
