open Contract_support

let files =
  [ "bin/kiln"; "include/kiln.h"; "lib/libkiln.a"; "lib/pkgconfig/kiln.pc" ]

let write path text =
  Out_channel.with_open_bin path (fun out -> Out_channel.output_string out text)

let command executable args = ignore (run executable args)

let executable path text =
  write path ("#!/bin/sh\nset -eu\n" ^ text);
  Unix.chmod path 0o755

let fixture test =
  temporary (fun root ->
      let at path = Filename.concat root path in
      List.iter
        (fun path -> command "mkdir" [ "-p"; at path ])
        [
          "commands";
          "assets";
          "package/bin";
          "package/include";
          "package/lib/pkgconfig";
          "installation with spaces/bin";
          "installation with spaces/include";
          "installation with spaces/lib/pkgconfig";
        ];
      executable (at "commands/uname")
        {|case "$1" in -s) echo "$TEST_SYSTEM";; -m) echo "$TEST_ARCH";; esac
|};
      executable (at "commands/sw_vers") {|echo "$TEST_VERSION"
|};
      executable (at "commands/curl")
        {|
[ "$TEST_FAILURE" != download ] || exit 1
output= url= format=
while [ "$#" -gt 0 ]; do
  case "$1" in -o) output=$2; shift;; -w) format=$2; shift;; https://*) url=$1;; esac
  shift
done
case "$url" in
  "https://github.com/$TEST_REPOSITORY/releases/latest")
    [ "$output" = /dev/null ] && [ "$format" = '%{url_effective}' ]
    printf 'https://github.com/%s/releases/tag/vtest' "$TEST_REPOSITORY";;
  "https://github.com/$TEST_REPOSITORY/releases/download/vtest/"*)
    cp "$ASSETS/${url##*/}" "$output";;
  *) exit 1;;
esac
|};
      test at)

let package at target missing =
  let included = List.filter (fun name -> Some name <> missing) files in
  List.iter
    (fun name -> write (at ("package/" ^ name)) (target ^ ":" ^ name))
    included;
  let archive = "kiln-" ^ target ^ ".tar.gz" in
  command "tar"
    ([ "-czf"; at ("assets/" ^ archive); "-C"; at "package" ] @ included);
  let digest =
    Digestif.SHA256.(to_hex (digest_string (read (at ("assets/" ^ archive)))))
  in
  write (at "assets/checksums.txt") (digest ^ "  " ^ archive ^ "\n");
  at ("assets/" ^ archive)

let install at ?(repository = "") system arch version failure =
  let settings =
    [
      ("PATH", at "commands" ^ ":" ^ Sys.getenv "PATH");
      ("ASSETS", at "assets");
      ("KILN_PREFIX", at "installation with spaces");
      ("KILN_REPOSITORY", repository);
      ( "TEST_REPOSITORY",
        if repository = "" then "hughhan1/kiln" else repository );
      ("TEST_SYSTEM", system);
      ("TEST_ARCH", arch);
      ("TEST_VERSION", version);
      ("TEST_FAILURE", failure);
    ]
  in
  let env = environment settings in
  let pid =
    Unix.create_process_env "sh"
      [| "sh"; "../install.sh" |]
      env Unix.stdin Unix.stdout Unix.stderr
  in
  snd (Unix.waitpid [] pid)

let () =
  List.iter
    (fun (system, arch, target) ->
      fixture (fun at ->
          ignore (package at target None);
          check target (install at system arch "15.0" "" = Unix.WEXITED 0);
          List.iter
            (fun name ->
              let path = at ("installation with spaces/" ^ name) in
              check ("installed " ^ name) (read path = target ^ ":" ^ name);
              check ("permissions " ^ name)
                ((Unix.stat path).Unix.st_perm
                = if name = "bin/kiln" then 0o755 else 0o644))
            files;
          let unrelated = at "installation with spaces/bin/unrelated" in
          write unrelated "keep me";
          check "reinstall from repository override"
            (install at ~repository:"example/kiln" system arch "15.0" ""
            = Unix.WEXITED 0);
          check "unrelated file preserved" (read unrelated = "keep me")))
    [
      ("Darwin", "arm64", "darwin_arm64");
      ("Darwin", "x86_64", "darwin_amd64");
      ("Linux", "aarch64", "linux_arm64");
      ("Linux", "x86_64", "linux_amd64");
    ];
  List.iter
    (fun failure ->
      fixture (fun at ->
          let archive =
            package at "linux_amd64"
              (if failure = "missing-file" then Some "lib/libkiln.a" else None)
          in
          if failure = "checksum" then write archive "corrupted archive";
          if failure = "missing-checksum" then
            write (at "assets/checksums.txt") "";
          List.iter
            (fun name ->
              write
                (at ("installation with spaces/" ^ name))
                "previous installation")
            files;
          let system, arch, version =
            match failure with
            | "unsupported" -> ("Linux", "riscv64", "15.0")
            | "old-macos" -> ("Darwin", "arm64", "14.7")
            | _ -> ("Linux", "x86_64", "15.0")
          in
          check ("reject " ^ failure)
            (install at system arch version failure <> Unix.WEXITED 0);
          List.iter
            (fun name ->
              check ("preserve " ^ name)
                (read (at ("installation with spaces/" ^ name))
                = "previous installation"))
            files))
    [
      "download";
      "checksum";
      "missing-file";
      "missing-checksum";
      "unsupported";
      "old-macos";
    ];
  print_endline
    "installer: platform selection, permissions, reinstall and failure \
     preservation passed"
