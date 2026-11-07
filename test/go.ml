module Planner = Kiln_planning.Planner
module Build_plan = Kiln_planning.Build_plan
module Pins = Kiln_providers.Pins
open Contract_support

let pins = List.map (fun pin -> pin.Pins.version) Pins.golang
let prepare root fields = Kiln_io.Driver.prepare (request root fields)

let compiler prepared =
  Yojson.Basic.Util.(
    prepared.Planner.resolution |> member "data" |> member "compiler"
    |> member "version")

let () =
  List.iter
    (fun pin ->
      snapshot
        [
          ("go.mod", "module example\n\ngo " ^ pin ^ "\n");
          ("server.go", "package main\nfunc main() {}");
        ]
        ("Go " ^ pin ^ " builds")
        (fun root ->
          let first = ok (prepare root []) in
          check "Go selected"
            (Yojson.Basic.Util.member "provider" first.Planner.resolution
            = `String "go");
          check "whole package startup"
            (Option.map Build_plan.arguments
               (Build_plan.startup first.Planner.plan)
            = Some [ "/app/bin/server" ]);
          check "the language version selects its patch line"
            (compiler first = `String pin)))
    pins;
  let oldest = List.hd pins and newest = last pins in
  snapshot
    [
      ( "go.mod",
        "module example\ngo " ^ oldest ^ "\ntoolchain go" ^ newest ^ "\n" );
    ]
    "an explicit toolchain selects its pin"
    (fun root ->
      check "explicit toolchain"
        (compiler (ok (prepare root [])) = `String newest));
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
        [
          ( "go.mod",
            "module example\ngo " ^ newest ^ "\ntoolchain go" ^ oldest ^ "\n" );
        ] );
      ("unsupported Go", [ ("go.mod", "module example\ngo 1.2.3\n") ]);
    ];
  finish ()
