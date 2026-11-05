open Contract_support

let entries path = Array.to_list (Sys.readdir path) |> List.sort String.compare
let copy source target = ignore (run "cp" [ "-Rf"; source ^ "/."; target ])

let prepare ?plan ~env binary source extra =
  let plan = Option.value ~default:(source ^ ".Dockerfile") plan in
  let options = [ "--plan-out"; plan; "--hide-pretty-plan" ] in
  ignore (run ~env binary ([ "prepare"; source ] @ options @ extra));
  plan

let container ?(build_args = []) ?(env = Unix.environment ()) ~rejected source
    plan =
  let tag = "kiln-smoke:" ^ Filename.basename (Filename.dirname source) in
  let docker args = run "docker" args in
  Fun.protect
    ~finally:(fun () ->
      try ignore (docker [ "image"; "rm"; "-f"; tag ])
      with Failure message -> prerr_endline message)
    (fun () ->
      let build = [ "buildx"; "build"; "--load"; "-t"; tag ] in
      ignore
        (run ~env
           ~exit_code:(if rejected then 1 else 0)
           "docker"
           (build @ build_args @ [ "-f"; plan; source ]));
      if rejected then []
      else
        let user =
          String.trim
            (docker [ "inspect"; "--format"; "{{.Config.User}}"; tag ])
        in
        check "non-root runtime"
          (match String.split_on_char ':' user with
          | [] | ("" | "0" | "root") :: _ -> false
          | _ :: _ -> true);
        check "application output"
          (String.trim (docker [ "run"; "--rm"; "--network=none"; tag ])
          = "kiln smoke");
        docker [ "inspect"; "--format"; "{{json .RootFS.Layers}}"; tag ]
        |> Yojson.Basic.from_string |> Yojson.Basic.Util.to_list)

let cli_arguments ~env binary source =
  let args = [ "node"; "-e"; "" ] in
  let options =
    [ "--provider"; "node" ]
    @ List.concat_map (fun arg -> [ "--start-arg"; arg ]) args
  in
  let plan = prepare ~env binary source options in
  let cmd =
    String.split_on_char '\n' (read plan)
    |> List.find (String.starts_with ~prefix:"CMD ")
  in
  check "CLI preserves flags and empty arguments"
    (Yojson.Basic.from_string (String.drop_first 4 cmd)
    = `List (List.map (fun arg -> `String arg) args))

let configured_commands ~env binary source =
  Sys.remove (Filename.concat source "package.json");
  write source "package.json"
    {|{"main":"main.js","scripts":{"build":"test -f before && printf inherited >> order"},"dependencies":{"local":"file:packages/local"}}|};
  write source "railpack.json"
    {|{
      "steps": {"build": {
        "assets": {"message": "configured $value \"quoted\"\nnext line", "script":"#!/bin/sh\nprintf executable"},
        "commands": [
          "RUN#before:touch before && printf before > order", "...",
          "FILE:-artifact message", "COPY:main.js copied.js",
          {"path":"/app/tool-script","name":"script","mode":493,"customName":"executable asset"},
          {"src":"main.js","dest":"object-copy.js"},
          {"src":"/etc/os-release","dest":"image-release","image":"docker.io/library/node:24-bookworm-slim"},
          "PATH:/app/first", {"path":"/app/$tools"},
          "RUN#check:test \"${PATH%%:*}\" = '/app/$tools'",
          {"cmd":"sh -c 'test -f copied.js && test -f object-copy.js && test -s image-release && test \"$(cat order)\" = beforeinherited && printf after >> order'", "customName":"ordered commands"},
          {"cmd":"node -e 'require(\"fs\").writeFileSync(\"args\", JSON.stringify(process.argv.slice(1)))' '' 'a b' \"quoted\" escaped\\ space '$literal'"}
        ]
      }},
      "deploy": {
        "startCommand": "test \"${PATH#*:}\" != \"$PATH\" && test \"${PATH%%:*}\" = '/app/$tools' && test \"$(cat ./-artifact)\" = \"$(printf '%s\\n%s' \"$MODE\" 'next line')\" && test \"$(./tool-script)\" = executable && test \"$(cat order)\" = beforeinheritedafter && test \"$(cat args)\" = '[\"\",\"a b\",\"quoted\",\"escaped space\",\"$literal\"]' && node copied.js",
        "variables": {"MODE": "configured $value \"quoted\""}
      }
    }|};
  let plan =
    prepare ~env binary source
      [ "--build-cmd"; "exit 99"; "--start-cmd"; "exit 99" ]
  in
  ignore (container ~rejected:false source plan)

let install_commands binary source name =
  write source "install-input" "installed";
  let python =
    if String.starts_with ~prefix:"python-" name then
      {|python -c 'import sys; assert sys.prefix == "/opt/venv"' && |}
    else ""
  in
  let variables =
    [
      ("RAILPACK_INSTALL_CMD", python ^ {|cp install-input install-marker|});
      ( "RAILPACK_BUILD_CMD",
        {|test "$(cat install-marker)" = installed && printf built > artifact|}
      );
      ( "RAILPACK_START_CMD",
        {|test "$(cat artifact)" = built && printf 'kiln smoke\n'|} );
    ]
  in
  let env = environment variables in
  let plan =
    prepare
      ~plan:(source ^ ".install.Dockerfile")
      ~env binary source
      (List.concat_map (fun (name, _) -> [ "-e"; name ]) variables)
  in
  ignore
    (container ~env
       ~build_args:
         (List.concat_map
            (fun (name, _) -> [ "--secret"; "id=" ^ name ^ ",env=" ^ name ])
            variables)
       ~rejected:false source plan)

let application_environment binary source =
  let secret = "private\nmultiline value" in
  let literal = "$HOME \\path \"quoted\"\tend" in
  write source "railpack.json"
    (Yojson.Basic.to_string
       (`Assoc
          [
            ( "deploy",
              `Assoc
                [
                  ("variables", `Assoc [ ("LITERAL", `String literal) ]);
                  ( "startCommand",
                    `String {|test -z "${TOKEN+x}" && node env.js|} );
                ] );
          ]));
  write source "env.js"
    ("if (process.env.LITERAL !== "
    ^ Yojson.Basic.to_string (`String literal)
    ^ ") process.exit(1); console.log('kiln smoke');\n");
  let missing = "KILN_MISSING_ENVIRONMENT_TEST" in
  let env =
    environment [ ("TOKEN", secret) ]
    |> Array.to_list
    |> List.filter (fun entry ->
        not (String.starts_with ~prefix:(missing ^ "=") entry))
    |> Array.of_list
  in
  let plan =
    prepare ~env binary source
      [
        "--env";
        missing;
        "-e";
        "TOKEN=overridden";
        "-e";
        "TOKEN";
        "--env";
        "TOKEN=";
        "--build-cmd";
        "test \"$TOKEN\" = \"$(printf 'private\\nmultiline value')\"";
      ]
  in
  ignore
    (container ~env
       ~build_args:[ "--secret"; "id=TOKEN,env=TOKEN" ]
       ~rejected:false source plan)

let generated_layers ~env binary source =
  write source "railpack.json" "{}";
  write source "package.json"
    {|{"main":"main.js","scripts":{"build":"node build.js"}}|};
  let packages =
    {|["node_modules/plain","node_modules/.pnpm/client@1/node_modules/client","node_modules/@scope/client","workspace/node_modules/client","generated/node_modules/client"]|}
  in
  write source "build.js"
    ("const packages = " ^ packages
   ^ {|;
const fs = require("node:fs");
const revision = fs.readFileSync("revision");
for (const pkg of packages) {
  fs.mkdirSync(pkg + "/generated", {recursive:true});
  fs.writeFileSync(pkg + "/ordinary.txt", "dependency");
  fs.writeFileSync(pkg + "/generated/client.txt", revision);
  fs.writeFileSync(pkg + "/generated/run", "#!/bin/sh\nprintf executable");
  fs.chmodSync(pkg, 0o750);
  fs.chmodSync(pkg + "/generated", 0o751);
  fs.chmodSync(pkg + "/generated/run", 0o750);
}
fs.symlinkSync(".pnpm/client@1/node_modules/client", "node_modules/client");
function normalize(path) {
  if (fs.lstatSync(path).isDirectory())
    for (const name of fs.readdirSync(path)) normalize(path + "/" + name);
  fs.lutimesSync(path, 0, 0);
}
normalize(".");
|}
    );
  Sys.remove (Filename.concat source "main.js");
  write source "main.js"
    ("const packages = " ^ packages
   ^ {|;
const fs = require("node:fs"), assert = require("node:assert/strict");
const {execFileSync} = require("node:child_process");
for (const pkg of packages) {
  assert.equal(fs.readFileSync(pkg + "/ordinary.txt", "utf8"), "dependency");
  assert.equal(fs.readFileSync(pkg + "/generated/client.txt", "utf8"), fs.readFileSync("revision", "utf8"));
  assert.equal(fs.statSync(pkg).mode & 0o777, 0o750);
  assert.equal(fs.statSync(pkg + "/generated").mode & 0o777, 0o751);
  assert.equal(fs.statSync(pkg + "/generated/client.txt").uid, 10001);
  assert.equal(execFileSync("./" + pkg + "/generated/run", {encoding:"utf8"}), "executable");
}
assert.equal(fs.readlinkSync("node_modules/client"), ".pnpm/client@1/node_modules/client");
assert.equal(fs.readFileSync("node_modules/client/generated/client.txt", "utf8"), fs.readFileSync("revision", "utf8"));
console.log("kiln smoke");
|}
    );
  let layers =
    List.map
      (fun revision ->
        write source "revision" revision;
        let plan = prepare ~env binary source [] in
        container ~rejected:false source plan)
      [ "one"; "two" ]
  in
  match layers with
  | [ before; after ] -> (
      match (List.rev before, List.rev after) with
      | source_before :: dependencies_before, source_after :: dependencies_after
        ->
          check "generated edits preserve dependency layers"
            (source_before <> source_after
            && dependencies_before = dependencies_after)
      | _ -> failwith "expected image layers")
  | _ -> assert false

let exercise binary name =
  snapshot [] name (fun root ->
      let original = "test/apps/" ^ name in
      let source = Filename.concat root "app" in
      copy original source;
      let empty = Filename.concat root "empty" in
      Unix.mkdir empty 0o700;
      let env = environment [ ("PATH", empty) ] in
      if name = "node-npm" then cli_arguments ~env binary source;
      let plan = prepare ~env binary source [] in
      ignore (container ~rejected:false source plan);
      ignore (run "diff" [ "-r"; original; source ]);
      install_commands binary source name;
      let rejected = "test/rejected/" ^ name in
      if Sys.file_exists rejected then (
        copy rejected source;
        ignore (container ~rejected:true source plan));
      if name = "node-npm" then (
        configured_commands ~env binary source;
        application_environment binary source;
        generated_layers ~env binary source))

let () =
  if Array.length Sys.argv < 2 then
    failwith "usage: integration CLI [APP ...] (from repository root)";
  let binary = Unix.realpath Sys.argv.(1) in
  let apps =
    if Array.length Sys.argv = 2 then entries "test/apps"
    else Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2))
  in
  List.iter (exercise binary) apps;
  finish ()
