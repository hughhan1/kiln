open Contract_support

let entries path = Array.to_list (Sys.readdir path) |> List.sort String.compare
let copy source target = ignore (run "cp" [ "-Rf"; source ^ "/."; target ])

let plan source =
  let dockerfile = source ^ ".Dockerfile" in
  Out_channel.with_open_bin dockerfile (fun channel ->
      Out_channel.output_string channel
        (Kiln_planning.Planner.dockerfile (ok (prepare source []))));
  dockerfile

let container ~rejected source plan =
  let tag = "kiln-smoke:" ^ Filename.basename (Filename.dirname source) in
  let docker args = run "docker" args in
  Fun.protect
    ~finally:(fun () ->
      try ignore (docker [ "image"; "rm"; "-f"; tag ])
      with Failure message -> prerr_endline message)
    (fun () ->
      ignore
        (run
           ~exit_code:(if rejected then 1 else 0)
           "docker"
           [ "buildx"; "build"; "--load"; "-t"; tag; "-f"; plan; source ]);
      if not rejected then (
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
          = "kiln smoke")))

let configured_commands source =
  Sys.remove (Filename.concat source "package.json");
  write source "package.json"
    {|{"main":"main.js","scripts":{"build":"test -f before && printf inherited >> order"},"dependencies":{"local":"file:packages/local"}}|};
  write source "railpack.json"
    {|{
      "steps": {"build": {
        "commands": [
          "RUN#before:touch before && printf before > order", "...",
          "COPY:main.js copied.js",
          {"src":"main.js","dest":"object-copy.js"},
          {"src":"/etc/os-release","dest":"image-release","image":"docker.io/library/node:24-bookworm-slim"},
          "PATH:/app/first", {"path":"/app/$tools"},
          "RUN#check:test \"${PATH%%:*}\" = '/app/$tools'",
          {"cmd":"sh -c 'test -f copied.js && test -f object-copy.js && test -s image-release && test \"$(cat order)\" = beforeinherited'", "customName":"ordered commands"},
          {"cmd":"node -e 'require(\"fs\").writeFileSync(\"args\", JSON.stringify(process.argv.slice(1)))' '' 'a b' \"quoted\" escaped\\ space '$literal'"},
          "RUN#arguments:test \"$(cat args)\" = '[\"\",\"a b\",\"quoted\",\"escaped space\",\"$literal\"]'"
        ]
      }}
    }|};
  container ~rejected:false source (plan source)

let exercise name =
  snapshot [] name (fun root ->
      let original = "test/apps/" ^ name in
      let source = Filename.concat root "app" in
      copy original source;
      let plan = plan source in
      container ~rejected:false source plan;
      ignore (run "diff" [ "-r"; original; source ]);
      let rejected = "test/rejected/" ^ name in
      if Sys.file_exists rejected then (
        copy rejected source;
        container ~rejected:true source plan);
      if name = "node-npm" then configured_commands source)

let () =
  List.iter exercise (entries "test/apps");
  finish ()
