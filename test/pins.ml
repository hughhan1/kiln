module Build_plan = Kiln_planning.Build_plan
module Python_version = Kiln_providers.Python_version
module Pins = Kiln_providers.Pins
module Semver = Kiln_core.Semver
open Contract_support

let semver text = Result.get_ok (Semver.version text)

let ascending compare versions =
  versions <> [] && List.sort_uniq compare versions = versions

let pinned images =
  List.iter
    (fun image ->
      check
        (image ^ " is pinned by digest")
        (Result.is_ok (Build_plan.image image)))
    images

let semver_table name versions =
  test (name ^ " versions ascend") (fun () ->
      check name (ascending Semver.compare (List.map semver versions)))

let python_table name versions =
  test (name ^ " versions ascend") (fun () ->
      check name
        (ascending Python_version.compare
           (List.map (fun v -> ok (Python_version.version v)) versions)))

let image_table parse name pins =
  parse name (List.map (fun pin -> pin.Pins.version) pins);
  test (name ^ " images") (fun () ->
      pinned (List.map (fun pin -> pin.Pins.image) pins))

let tool_table name tools =
  semver_table name (List.map (fun tool -> tool.Pins.tool) tools);
  test (name ^ " engines") (fun () ->
      List.iter
        (fun tool ->
          check tool.Pins.engines
            (Result.is_ok (Semver.range tool.Pins.engines)))
        tools)

let toolchains name pins =
  test (name ^ " images") (fun () ->
      pinned
        (List.concat_map
           (fun pin -> [ pin.Pins.builder; pin.Pins.runtime ])
           pins))

let () =
  let nodes = List.map (fun pin -> pin.Pins.node) Pins.node in
  semver_table "Node" (List.map (fun pin -> pin.Pins.release) nodes);
  toolchains "Node" nodes;
  test "a default Node release" (fun () ->
      check "an LTS release is pinned"
        (List.exists (fun pin -> Option.is_some pin.Pins.lts) Pins.node);
      List.iter (fun pin -> ignore (semver pin.Pins.npm)) Pins.node);
  tool_table "npm" Pins.npm;
  tool_table "pnpm" Pins.pnpm;
  python_table "Python" (List.map (fun pin -> pin.Pins.release) Pins.python);
  toolchains "Python" Pins.python;
  image_table python_table "uv" Pins.uv;
  image_table semver_table "Bun" Pins.bun;
  image_table semver_table "Deno" Pins.deno;
  image_table semver_table "Go" Pins.golang;
  test "Debian image" (fun () -> pinned [ Pins.debian ]);
  finish ()
