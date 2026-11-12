module Build_plan = Kiln_planning.Build_plan
module Json = Kiln_planning.Json
module Planner = Kiln_planning.Planner
module Provider = Kiln_planning.Provider
module Pins = Kiln_providers.Pins
open Contract_support

let files = [ ("package.json", {|{"main":"main.js"}|}); ("main.js", "") ]
let oldest_node = (List.hd Pins.node).Pins.node.Pins.release
let recipe root fields = Planner.dockerfile (ok (prepare root fields))

let () =
  List.iter
    (fun (provider, files) ->
      snapshot files (provider ^ " optional startup") (fun root ->
          let prepared = ok (prepare root []) in
          let open Yojson.Basic.Util in
          let response = Yojson.Basic.from_string (Planner.encode prepared) in
          check "inherited image command cleared"
            (contains (member "dockerfile" response |> to_string) "CMD []");
          check "missing startup warning"
            (match member "logs" response |> to_list with
            | [ log ] ->
                member "Level" log = `String "warn"
                && contains (member "Msg" log |> to_string) "no startup"
            | [] | _ :: _ -> false);
          check "strict mode rejects a missing startup"
            (prepare root [ ("errorMissingStartCommand", `Bool true) ]
            = Error Kiln_planning.Error.Missing_start);
          let launched =
            ok
              (prepare root
                 [
                   ("start", Json.string_array [ "/launcher" ]);
                   ("errorMissingStartCommand", `Bool true);
                 ])
          in
          check "declared startup has no warning"
            (member "logs" (Yojson.Basic.from_string (Planner.encode launched))
            = `List [])))
    [
      ("Node", [ ("package.json", "{}") ]);
      ("empty Node entry", [ ("package.json", {|{"main":""}|}) ]);
      ("Bun", [ ("package.json", {|{"packageManager":"bun@1"}|}) ]);
      ("Deno", [ ("deno.json", "{}") ]);
      ("Python", [ ("requirements.txt", "") ]);
    ];
  snapshot files "environment version overrides a version file" (fun root ->
      write root ".node-version" (last Pins.node).Pins.node.Pins.release;
      let prepared =
        ok
          (prepare root
             [
               ( "environment",
                 `Assoc [ ("RAILPACK_NODE_VERSION", `String oldest_node) ] );
             ])
      in
      let open Yojson.Basic.Util in
      check "environment overrides file version"
        (prepared.Planner.resolution |> member "data" |> member "nodeVersion"
       = `String oldest_node));
  snapshot files "provider registration and selection" (fun root ->
      let module Alternate = struct
        include Kiln_providers.Node

        let id = "alternate"
        let public_name = id
        let detect _ = Ok Provider.No_match
      end in
      let providers : Provider.t list =
        [ (module Kiln_providers.Node); (module Alternate) ]
      in
      let explicit =
        ok (prepare ~providers root [ ("provider", `String "alternate") ])
      in
      check "explicit provider selected"
        (Yojson.Basic.Util.member "provider" explicit.Planner.resolution
        = `String "alternate");
      check "detection is independent of the selected implementation"
        (Yojson.Basic.Util.member "detectedProviders"
           (`Assoc (Planner.info explicit))
        = `List [ `String "node" ]);
      check "unknown provider rejected"
        (Result.is_error (prepare root [ ("provider", `String "unknown") ])));
  snapshot [] "unknown application" (fun root ->
      check "no implicit application fixture"
        (Result.is_error (prepare root [])));
  test "startup inheritance" (fun () ->
      let image =
        ok (Build_plan.image ("example/image@sha256:" ^ String.make 64 'a'))
      in
      let args = [ "program" ] in
      let command = ok (Build_plan.argv args) in
      let parent =
        ok
          (Build_plan.stage (Build_plan.Image image)
             [ Build_plan.Start (Some command) ])
      in
      let other = ok (Build_plan.argv [ "other" ]) in
      List.iter
        (fun (label, operations, expected) ->
          let child =
            ok (Build_plan.stage (Build_plan.Stage parent) operations)
          in
          check label
            (Option.map Build_plan.arguments (Build_plan.startup child)
            = expected))
        [
          ("inherit startup", [], Some args);
          ( "entrypoint clears inherited CMD",
            [ Build_plan.Entrypoint None ],
            None );
          ( "entrypoint preserves local CMD",
            [ Build_plan.Start (Some other); Build_plan.Entrypoint None ],
            Some [ "other" ] );
          ( "last local CMD wins",
            [
              Build_plan.Entrypoint None;
              Build_plan.Start (Some command);
              Build_plan.Start (Some other);
            ],
            Some [ "other" ] );
        ]);
  snapshot
    (files
    @ [
        ( "railpack.json",
          {|{
    // Shared configuration, including JSONC syntax.
    "provider": "node",
    "deploy": {"startCommand": "cat artifact", "variables": {"MODE":"configured"}},
    "secrets": ["FILE", "FILE"],
  }|}
        );
      ])
    "shared preparation contract"
    (fun root ->
      let first =
        ok
          (prepare root
             [
               ( "environment",
                 `Assoc
                   [
                     ("RAILPACK_BUILD_CMD", `String "echo environment-build");
                     ("RAILPACK_START_CMD", `String "echo environment-start");
                     ("TOKEN", `String "private");
                   ] );
             ])
      in
      let recipe = Planner.dockerfile first in
      check "secret values stay out of prepared output"
        (not (contains (Planner.encode first) "private"));
      check "file startup takes precedence"
        (contains recipe {|CMD ["/bin/bash","-c","cat artifact"]|});
      check "runtime variables" (contains recipe {|MODE="configured"|});
      let response = Yojson.Basic.from_string (Planner.encode first) in
      let open Yojson.Basic.Util in
      let version =
        read "../kiln_ocaml.opam" |> String.split_on_char '\n'
        |> List.find (String.starts_with ~prefix:"version: ")
        |> String.drop_first 10 |> String.drop_last 1
      in
      check "result identifies the package version"
        (member "kilnVersion" response = `String version);
      check "configured and supplied secrets form a set"
        (response |> member "secrets"
        = Json.string_array
            [ "FILE"; "RAILPACK_BUILD_CMD"; "RAILPACK_START_CMD"; "TOKEN" ]);
      let argv = [ "/launcher"; "with spaces"; "\"quoted\""; "" ] in
      let native =
        ok
          (prepare root
             [
               ("start", Json.string_array argv);
               ( "environment",
                 `Assoc
                   [ ("RAILPACK_START_CMD", `String "echo environment-start") ]
               );
             ])
      in
      let dockerfile =
        Yojson.Basic.from_string (Planner.encode native)
        |> member "dockerfile" |> to_string
      in
      check
        "native launcher argv overrides application startup without shell \
         interpretation"
        (contains dockerfile ("CMD " ^ Json.encode (Json.string_array argv))));
  snapshot
    [
      ( "package.json",
        {|{"main":"main.js","scripts":{"build":"echo inherited"}}|} );
    ]
    "build command precedence and inheritance"
    (fun root ->
      List.iter
        (fun (commands, expected) ->
          write root "railpack.json"
            ("{\"steps\":{\"build\":{" ^ commands ^ "}}}");
          let prepared =
            ok (prepare root [ ("buildCommand", `String "echo request") ])
          in
          let recipe = Build_plan.dockerfile prepared.Planner.plan in
          List.iter
            (fun (line, selected) ->
              check (commands ^ ": " ^ line) (contains recipe line = selected))
            [
              ({|RUN ["/bin/sh","-c","echo request"]|}, expected = `Request);
              ({|RUN ["npm","run","build"]|}, expected = `Inherit);
            ])
        [
          ("", `Request);
          ({|"commands":[]|}, `Empty);
          ({|"commands":["..."]|}, `Inherit);
          ({|"commands":[{"cmd":"..."}]|}, `Inherit);
          ({|"commands":[{"cmd":"sh -c '...'"}]|}, `Inherit);
        ]);
  snapshot files "inherited install follows earlier commands" (fun root ->
      write root "railpack.json"
        {|{"steps":{"install":{"commands":["echo before","..."]}}}|};
      let recipe = recipe root [] in
      let position line = Option.get (String.find_first ~sub:line recipe) in
      check "inherited install follows earlier commands"
        (position {|RUN ["/bin/sh","-c","echo before"]|}
        < position {|"npm","install"]|}));
  snapshot files "invalid configuration is rejected" (fun root ->
      let reject config =
        write root "railpack.json" config;
        check config (Result.is_error (prepare root []))
      in
      List.iter reject
        [
          {|{"deploy":{"aptPackages":["curl"]}}|};
          {|{"steps":{"build":{"inputs":[]}}}|};
          {|{"steps":{"install":{"assets":{}}}}|};
          {|{"steps":{"install":{"commands":["FILE:artifact missing"]}}}|};
          {|{"steps":{"build":{"commands":[{"cmd":"'unclosed"}]}}}|};
          {|{"steps":{"build":{"commands":[{"cmd":"echo ignored","path":"/tools"}]}}}|};
          {|{"steps":{"build":{"commands":["COPY:only-source"]}}}|};
          {|{"steps":{"build":{"commands":[{"src":"","dest":"copy.js"}]}}}|};
          {|{"steps":{"build":{"commands":[{"src":"main.js","dest":"copy.js","image":"bad image"}]}}}|};
          {|{"steps":{"build":{"assets":{"x":"value"},"commands":[{"path":"artifact","name":"x","mode":4096}]}}}|};
          {|{"steps":{"build":{"commands":["PATH:/tools\rINJECT"]}}}|};
          {|{"steps":{"build":{"commands":[" "]}}}|};
          {|{"deploy":{"variables":{"BAD KEY":"value"}}}|};
          {|{"deploy":{"variables":{"MODE":"line\nbreak"}}}|};
          {|{"secrets":["BAD KEY"]}|};
        ]);
  snapshot files "invalid request is rejected" (fun root ->
      List.iter
        (fun fields ->
          check "invalid request rejected"
            (Result.is_error (prepare root fields)))
        [
          [ ("environment", `Assoc [ ("BAD KEY", `String "value") ]) ];
          [ ("environment", `Assoc [ ("TOKEN", `String "bad\000value") ]) ];
          [
            ("buildCommand", `Int 42);
            ( "environment",
              `Assoc [ ("RAILPACK_BUILD_CMD", `String "echo build") ] );
          ];
          [
            ("start", Json.string_array [ "node" ]);
            ("startCommand", `String "echo bad");
          ];
          [ ("start", `List []) ];
          [ ("start", `List [ `String "" ]) ];
          [ ("start", `List [ `String "node"; `String "\000" ]) ];
          [ ("errorMissingStartCommand", `String "true") ];
          [ ("platforms", `List []) ];
          [ ("platforms", `List [ `String "darwin/arm64" ]) ];
        ]);
  snapshot files "CLI rejects invalid environment names" (fun root ->
      List.iter
        (fun entry ->
          let errors = Buffer.create 64 in
          ignore
            (run ~exit_code:1 "../bin/main.exe" ~errors
               [ "prepare"; root; "-e"; entry; "--hide-pretty-plan" ]);
          check "invalid name diagnosed"
            (contains (Buffer.contents errors) "invalid variable name"))
        [ "=value" ]);
  snapshot
    [ ("package.json", "{}") ]
    "CLI failure"
    (fun root ->
      let info = Filename.concat root "info.json" in
      let output =
        run ~exit_code:1 "../bin/main.exe"
          [
            "prepare";
            root;
            "--info-out";
            info;
            "--hide-pretty-plan";
            "--error-missing-start";
          ]
      in
      check "failed CLI emits no plan" (output = "");
      let open Yojson.Basic.Util in
      let info = Yojson.Basic.from_file info in
      check "failure" (member "success" info = `Bool false);
      check "structured diagnostic"
        (match member "logs" info |> to_list with
        | [ log ] ->
            member "Level" log = `String "error"
            && contains (member "Msg" log |> to_string) "no startup"
        | [] | _ :: _ -> false));
  List.iter
    (fun (provider, files, ignored) ->
      snapshot files (provider ^ " ignores node_modules") (fun root ->
          Unix.mkdir (Filename.concat root "node_modules") 0o700;
          write root ("node_modules/" ^ ignored) "[";
          check "it stays out of planning and the build context"
            (contains (recipe root []) "--exclude=node_modules")))
    [
      ("Python", [ ("main.py", "") ], "pyproject.toml");
      ("Go", [ ("go.mod", "module example\ngo 1.27\n") ], "go.work");
    ];
  snapshot files "contained custom configuration" (fun root ->
      Unix.mkdir (Filename.concat root "config") 0o700;
      write root "config/build.json"
        {|{"deploy":{"startCommand":"echo nested"}}|};
      let plan =
        ok (prepare root [ ("configFilePath", `String "config/build.json") ])
      in
      check "custom path used"
        (contains (Build_plan.dockerfile plan.Planner.plan) "echo nested"));
  let project =
    {|[project]
name = "app"
version = "1"
dependencies = ["idna==3.10"]
|}
  in
  let development = [ ("development", `Bool true) ] in
  List.iter
    (fun (name, files, source, project) ->
      snapshot files (name ^ " development image leaves source out")
        (fun root ->
          let plain = recipe root [] and image = recipe root development in
          check "only the flag excludes source"
            ((not (contains plain source)) && contains image source);
          Option.iter
            (fun (marker, in_development) ->
              check "the project is not installed"
                (contains plain marker <> in_development
                && contains image marker = in_development))
            project))
    [
      ( "pip",
        [
          ("pyproject.toml", project);
          ("main.py", "");
          ("package.json", {|{"scripts":{"build":"vite build"}}|});
        ],
        "--exclude=**/*.py ",
        Some ("--no-deps", false) );
      ( "Poetry",
        [
          ( "pyproject.toml",
            project
            ^ "[build-system]\nbuild-backend = \"poetry.core.masonry.api\"\n" );
          ("main.py", "");
        ],
        "--exclude=**/*.py ",
        Some ("--no-root", true) );
      ( "Node",
        [ ("package.json", {|{"main":"[id].js"}|}); ("[id].js", "") ],
        "--exclude=?id?.js ",
        None );
    ];
  snapshot files "development exclusions fit BuildKit's line limit" (fun root ->
      for i = 1 to 2000 do
        write root
          (Printf.sprintf "%04d-a-rather-long-source-file-name.js" i)
          ""
      done;
      check "every line fits"
        (List.for_all
           (fun line -> String.length line < 65536)
           (String.split_on_char '\n' (recipe root development))));
  finish ()
