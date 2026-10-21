module Planner = Kiln_planning.Planner
module Build_plan = Kiln_planning.Build_plan
open Contract_support

let fetch =
  registry
    [
      ("library/golang", [ "1.26.8"; "1.27.0"; "1.27.2"; "1.28.0" ]);
      ("library/debian", [ "bookworm-slim" ]);
    ]

let prepare ?(fetch = fetch) root fields =
  Kiln_io.Driver.prepare_with ~today:"2026-10-20" ~fetch (request root fields)

let () =
  snapshot
    [
      ("go.mod", "module example\n\ngo 1.27.0\n");
      ("server.go", "package main\nfunc main() {}");
    ]
    "Go package builds"
    (fun root ->
      let first = ok (prepare root []) in
      check "Go selected"
        (Yojson.Basic.Util.member "provider" first.Planner.resolution
        = `String "go");
      check "whole package startup"
        (Option.map Build_plan.arguments (Build_plan.startup first.Planner.plan)
        = Some [ "/app/bin/server" ]);
      check "latest matching patch"
        (Yojson.Basic.Util.(
           first.Planner.resolution |> member "data" |> member "compiler"
           |> member "version")
        = `String "1.27.2");
      write root "helper.go" "package main\nconst message = \"hello\"\n";
      let reused =
        ok
          (prepare
             ~fetch:(fun _ _ -> failwith "offline discovery")
             root
             [ ("resolution", first.Planner.resolution) ])
      in
      check "source edits retain toolchain resolution"
        (reused.Planner.resolution = first.Planner.resolution);
      write root "go.mod" "module example\ngo 1.27.0\ntoolchain go1.28.0\n";
      let upgraded = ok (prepare root []) in
      check "explicit toolchain"
        (Yojson.Basic.Util.(
           upgraded.Planner.resolution |> member "data" |> member "compiler"
           |> member "version")
        = `String "1.28.0"));
  List.iter
    (fun (label, files) ->
      snapshot files label (fun root ->
          check "unsupported module rejected"
            (Result.is_error (prepare root []))))
    [
      ( "workspace",
        [ ("go.mod", "module example\ngo 1.27\n"); ("go.work", "go 1.27\n") ] );
      ("missing Go version", [ ("go.mod", "module example\n") ]);
      ( "toolchain below language minimum",
        [ ("go.mod", "module example\ngo 1.28\ntoolchain go1.27.2\n") ] );
    ];
  finish ()
