module Build_plan = Kiln_planning.Build_plan
module Pins = Kiln_providers.Pins
open Contract_support

let oldest = (List.hd Pins.golang).Pins.version
let newest = (last Pins.golang).Pins.version

let go_mod language toolchain =
  "module example\ngo " ^ language ^ "\ntoolchain go" ^ toolchain ^ "\n"

let () =
  snapshot
    [ ("go.mod", go_mod oldest newest); ("server.go", "package main") ]
    "an explicit toolchain builds the whole package"
    (fun root ->
      let dockerfile = Build_plan.dockerfile (ok (prepare root [])) in
      check "whole package startup"
        (contains dockerfile {|CMD ["/app/bin/server"]|});
      check "the toolchain line wins over the go line"
        (contains dockerfile (last Pins.golang).Pins.image));
  snapshot
    [
      ("go.mod", go_mod oldest newest);
      ("server.go", "package main");
      ("package.json", {|{"scripts":{"build":"vite build"}}|});
    ]
    "a package.json build runs before go build, without Go's files"
    (fun root ->
      let dockerfile = Build_plan.dockerfile (ok (prepare root [])) in
      check "frontend, then go build"
        (ordered
           (String.split_on_char '\n' dockerfile)
           [
             "FROM --platform=$BUILDPLATFORM docker.io/library/node";
             {|RUN ["npm","run","build"]|};
             "COPY --from=";
             {|RUN ["/bin/sh","-c","test|};
           ]);
      check "the frontend sees no Go files and its output no node_modules"
        (contains dockerfile "--exclude=*.go"
        && contains dockerfile {|--exclude=**/node_modules ["/app/","/app/"]|}));
  List.iter
    (fun (label, files) ->
      snapshot files label (fun root ->
          check "unsupported module rejected"
            (Result.is_error (prepare root []))))
    [
      ( "workspace",
        [ ("go.mod", "module example\ngo 1.27\n"); ("go.work", "go 1.27\n") ] );
      ("missing Go version", [ ("go.mod", "module example\n") ]);
      ("toolchain below language minimum", [ ("go.mod", go_mod newest oldest) ]);
    ];
  finish ()
