module Build_plan = Kiln_planning.Build_plan
module Json = Kiln_planning.Json
module Planner = Kiln_planning.Planner
module Provider = Kiln_planning.Provider
open Contract_support

let files = [ ("package.json", {|{"main":"main.js"}|}); ("main.js", "") ]

let prepare ?providers ?(fetch = Node_fixture.fetch) root fields =
  Kiln_io.Driver.prepare_with ?providers ~today:"2026-10-11" ~fetch
    (request root fields)

let () =
  let fetch =
    registry
      [
        ("oven/bun", [ "1.3.1" ]);
        ("denoland/deno", [ "2.5.1" ]);
        ("library/python", [ "3.13.7-trixie"; "3.13.7-slim-trixie" ]);
      ]
  in
  List.iter
    (fun (provider, files, fetch) ->
      snapshot files (provider ^ " optional startup") (fun root ->
          let prepared = ok (prepare ~fetch root []) in
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
          check "strict mode rejects before discovery"
            (prepare
               ~fetch:(fun _ _ ->
                 failwith "missing startup performed discovery")
               root
               [ ("errorMissingStartCommand", `Bool true) ]
            = Error Kiln_core.Error.Missing_start);
          let argv = [ "/launcher"; "" ] in
          let launched =
            ok
              (prepare ~fetch root
                 [
                   ("start", Json.string_array argv);
                   ("errorMissingStartCommand", `Bool true);
                   ("resolution", prepared.Planner.resolution);
                 ])
          in
          let response = Yojson.Basic.from_string (Planner.encode launched) in
          check "strict native launcher preserves argv"
            (contains
               (member "dockerfile" response |> to_string)
               {|CMD ["/launcher",""]|});
          check "declared startup has no warning"
            (member "logs" response = `List [])))
    [
      ("Node", [ ("package.json", "{}") ], Node_fixture.fetch);
      ( "empty Node entry",
        [ ("package.json", {|{"main":""}|}) ],
        Node_fixture.fetch );
      ("Bun", [ ("package.json", {|{"packageManager":"bun@1.3.1"}|}) ], fetch);
      ("Deno", [ ("deno.json", "{}") ], fetch);
      ("Python", [ ("requirements.txt", "") ], fetch);
    ];
  snapshot files "application environment and secret contract" (fun root ->
      write root ".node-version" "24";
      let prepared =
        ok
          (prepare root
             [
               ( "environment",
                 `Assoc
                   [
                     ("RAILPACK_NODE_VERSION", `String "22.20.0");
                     ("TOKEN", `String "private\nvalue");
                   ] );
             ])
      in
      let open Yojson.Basic.Util in
      check "environment overrides file version"
        (prepared.Planner.resolution |> member "data" |> member "nodeVersion"
       = `String "22.20.0");
      check "secret values stay out of prepared output"
        (not (contains (Planner.encode prepared) "private")));
  snapshot files "legacy resolution reuse" (fun root ->
      let first = ok (prepare root []) in
      let legacy = Yojson.Basic.Util.member "data" first.Planner.resolution in
      let reused =
        ok
          (Kiln_io.Driver.prepare_with ~today:"2040-01-01"
             ~fetch:(fun _ _ -> failwith "resolution reuse performed discovery")
             (request root [ ("resolution", legacy) ]))
      in
      check "legacy pins preserved"
        (reused.Planner.resolution = first.Planner.resolution);
      let malformed =
        `Assoc
          [
            ("format", `Int 1);
            ("provider", `String "node");
            ("key", `String "old");
            ("data", `Null);
          ]
      in
      let fields = Yojson.Basic.Util.to_assoc legacy in
      let mutable_image =
        `Assoc
          (("builderImage", `String "example/image:latest")
          :: List.remove_assoc "builderImage" fields)
      in
      List.iter
        (fun resolution ->
          check "malformed or unpinned resolution rejected"
            (Result.is_error (prepare root [ ("resolution", resolution) ])))
        [ malformed; mutable_image ]);
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
          ( "explicit clear wins",
            [ Build_plan.Start (Some other); Build_plan.Start None ],
            None );
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
    "steps": {"build": {"commands": ["printf built > artifact"]}},
    "deploy": {"startCommand": "cat artifact", "variables": {"MODE":"configured"}},
    "secrets": ["TOKEN", "TOKEN"],
  }|}
        );
      ])
    "shared preparation contract"
    (fun root ->
      let first =
        ok
          (prepare root
             [
               ("buildCommand", `String "printf override > artifact");
               ("startCommand", `String "printf '%s' overridden");
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
      check "file build command takes precedence"
        (contains recipe {|"printf built > artifact"|}
        && (not (contains recipe "printf override"))
        && not (contains recipe "echo environment-build"));
      check "file startup takes precedence"
        (contains recipe {|CMD ["/bin/bash","-c","cat artifact"]|});
      check "runtime variables" (contains recipe {|MODE="configured"|});
      let response = Yojson.Basic.from_string (Planner.encode first) in
      let open Yojson.Basic.Util in
      check "result success" (response |> member "success" = `Bool true);
      let version =
        read "../kiln_ocaml.opam" |> String.split_on_char '\n'
        |> List.find (String.starts_with ~prefix:"version: ")
        |> String.drop_first 10 |> String.drop_last 1
      in
      check "result identifies the package version"
        (member "kilnVersion" response = `String version
        && member "railpackVersion" response = `Null);
      check "provider metadata is a string map"
        (response |> member "metadata"
        |> member "nodePackageManager"
        = `String "npm");
      check "provider metadata"
        (response |> member "detectedProviders" = `List [ `String "node" ]);
      check "configured and supplied secrets form a set"
        (response |> member "secrets"
        = Json.string_array
            [ "RAILPACK_BUILD_CMD"; "RAILPACK_START_CMD"; "TOKEN" ]);
      check "resolved package schema"
        (response |> member "resolvedPackages" |> member "node"
       |> member "resolvedVersion" = `String "24.0.0");
      let argv = [ "/launcher"; "with spaces"; "\"quoted\""; "" ] in
      let native =
        ok
          (prepare root
             [
               ("start", Json.string_array argv);
               ("resolution", first.Planner.resolution);
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
        (contains dockerfile ("CMD " ^ Json.encode (Json.string_array argv)));
      check "argv and shell conflict"
        (Result.is_error
           (prepare root
              [
                ("start", Json.string_array [ "node" ]);
                ("startCommand", `String "echo bad");
              ])));
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
  snapshot files "install configuration is accepted" (fun root ->
      List.iter
        (fun config ->
          write root "railpack.json" config;
          check config (Result.is_ok (prepare root [])))
        [
          {|{"steps":{"install":{}}}|};
          {|{"steps":{"install":{"commands":[]}}}|};
          {|{"steps":{"install":{"commands":["echo before","..."]}}}|};
        ];
      let recipe = Planner.dockerfile (ok (prepare root [])) in
      let position line = Option.get (String.find_first ~sub:line recipe) in
      check "inherited install follows earlier commands"
        (position {|RUN ["/bin/sh","-c","echo before"]|}
        < position {|RUN ["npm","install"]|}));
  snapshot files "invalid configuration fails before discovery" (fun root ->
      let reject config =
        write root "railpack.json" config;
        check config
          (Result.is_error
             (Kiln_io.Driver.prepare_with ~today:"2026-10-12"
                ~fetch:(fun _ _ ->
                  failwith "invalid config performed discovery")
                (request root [])))
      in
      List.iter reject
        [
          {|{"deploy":{"aptPackages":["curl"]}}|};
          {|{"steps":{"build":{"inputs":[]}}}|};
          {|{"steps":{"install":{"assets":{}}}}|};
          {|{"steps":{"install":{"commands":["FILE:artifact missing"]}}}|};
          {|{"steps":{"build":{"commands":[{"cmd":"'unclosed"}]}}}|};
          {|{"steps":{"build":{"commands":[{"cmd":"echo ignored","path":"/tools"}]}}}|};
          {|{"steps":{"build":{"commands":[{"cmd":"echo ok","unknown":true}]}}}|};
          {|{"steps":{"build":{"commands":["COPY:only-source"]}}}|};
          {|{"steps":{"build":{"commands":[{"src":"","dest":"copy.js"}]}}}|};
          {|{"steps":{"build":{"commands":[{"src":"main.js","dest":"copy.js","image":"bad image"}]}}}|};
          {|{"steps":{"build":{"assets":{"x":"value"},"commands":[{"path":"artifact","name":"x","mode":4096}]}}}|};
          {|{"steps":{"build":{"commands":["PATH:/tools\rINJECT"]}}}|};
          {|{"steps":{"build":{"commands":["FILE:artifact missing"]}}}|};
          {|{"steps":{"build":{"commands":[" "]}}}|};
          {|{"steps":{"build":{"commands":["echo\u0000bad"]}}}|};
          {|{"deploy":{"variables":{"BAD KEY":"value"}}}|};
          {|{"deploy":{"variables":{"MODE":"line\nbreak"}}}|};
          {|{"secrets":["BAD KEY"]}|};
          {|{"deploy":{"variables":{"MODE":"line\rbreak"}}}|};
        ]);
  snapshot files "invalid request fails before discovery" (fun root ->
      List.iter
        (fun fields ->
          check "invalid request rejected"
            (Result.is_error
               (Kiln_io.Driver.prepare_with ~today:"2026-10-12"
                  ~fetch:(fun _ _ ->
                    failwith "invalid request performed discovery")
                  (request root fields))))
        [
          [ ("environment", `Assoc [ ("BAD KEY", `String "value") ]) ];
          [ ("environment", `Assoc [ ("TOKEN", `String "bad\000value") ]) ];
          [
            ("buildCommand", `Int 42);
            ( "environment",
              `Assoc [ ("RAILPACK_BUILD_CMD", `String "echo build") ] );
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
        [ "BAD KEY"; "BAD KEY="; "=value" ]);
  let proxy = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close proxy)
    (fun () ->
      Unix.bind proxy (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      let port =
        match Unix.getsockname proxy with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> failwith "expected TCP socket"
      in
      let env =
        environment
          [
            ("HTTPS_PROXY", Printf.sprintf "http://127.0.0.1:%d" port);
            ("NO_PROXY", "");
            ("no_proxy", "");
          ]
      in
      List.iter
        (fun (label, files, flags, code, message) ->
          snapshot files ("CLI failure: " ^ label) (fun root ->
              let info = Filename.concat root "info.json" in
              let output =
                run ~env ~exit_code:code "../bin/main.exe"
                  ([ "prepare"; root; "--info-out"; info; "--hide-pretty-plan" ]
                  @ flags)
              in
              check "failed CLI emits no plan" (output = "");
              let open Yojson.Basic.Util in
              let info = Yojson.Basic.from_file info in
              check "failure" (member "success" info = `Bool false);
              check "structured diagnostic"
                (match member "logs" info |> to_list with
                | [ log ] ->
                    member "Level" log = `String "error"
                    && contains (member "Msg" log |> to_string) message
                | [] | _ :: _ -> false)))
        [
          ( "configuration",
            files @ [ ("railpack.json", {|{"buildAptPackages":["curl"]}|}) ],
            [],
            1,
            "buildAptPackages" );
          ("temporary discovery", files, [], 75, "toolchain discovery failed:");
          ( "strict startup",
            [ ("package.json", "{}") ],
            [ "--error-missing-start" ],
            1,
            "no startup" );
        ]);
  snapshot files "previous versions replace defaults" (fun root ->
      let previous = `Assoc [ ("node", `String "22.20.0") ] in
      let first = ok (prepare root [ ("previousVersions", previous) ]) in
      let open Yojson.Basic.Util in
      let package prepared =
        `Assoc (Planner.info prepared)
        |> member "resolvedPackages" |> member "node"
      in
      let check_package prepared source version =
        let package = package prepared in
        check "request selects the reported version and origin"
          (member "resolvedVersion" package = `String version
          && member "source" package = `String source
          && member "requestedVersion" package = `String version)
      in
      check_package first "previousVersions" "22.20.0";
      write root ".node-version" "24.0.0";
      check_package
        (ok (prepare root [ ("previousVersions", previous) ]))
        ".node-version" "24.0.0");
  snapshot files "contained custom configuration" (fun root ->
      Unix.mkdir (Filename.concat root "config") 0o700;
      write root "config/build.json"
        {|{"deploy":{"startCommand":"echo nested"}}|};
      let plan =
        ok (prepare root [ ("configFilePath", `String "config/build.json") ])
      in
      check "custom path used"
        (contains (Build_plan.dockerfile plan.Planner.plan) "echo nested"));
  finish ()
