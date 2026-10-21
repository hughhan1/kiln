(* Compatibility fixtures in node-semver.json come from npm/node-semver v7.7.2:
   https://github.com/npm/node-semver/tree/v7.7.2
   License retained in test/node-semver.LICENSE. *)
module Semver = Kiln_core.Semver
open Contract_support

let semver_cases () =
  (* The OCaml API accepts strings; non-string inputs are rejected by its type. *)
  let open Yojson.Basic.Util in
  let fixtures =
    Yojson.Basic.from_file "node-semver.json"
    |> to_list
    |> List.map (fun case ->
        ( case |> member "range" |> to_string,
          case |> member "version" |> to_string,
          case |> member "want" |> to_bool,
          member "validVersion" case <> `Bool false ))
  in
  let prereleases =
    [
      ("", "1.0.0-beta", false, true);
      ("1.0.0-beta || >0", "1.0.0-alpha", false, true);
      (">=1.0.0-beta.2 <1.0.0", "1.0.0-beta.3", true, true);
      ("1.0.0-beta.2", "1.0.0-beta.2", true, true);
    ]
  in
  List.iter
    (fun (range, version, expected, valid) ->
      let parsed_range =
        Result.fold ~ok:Fun.id ~error:failwith (Semver.range range)
      in
      let parsed_version = Semver.version version in
      let label = Printf.sprintf "%S / %S" range version in
      check ("version validity: " ^ label) (Result.is_ok parsed_version = valid);
      check
        ("range membership: " ^ label)
        (Result.fold
           ~ok:(Semver.matches parsed_range)
           ~error:(fun _ -> false)
           parsed_version
        = expected))
    (fixtures @ prereleases);
  List.iter
    (fun bad ->
      check ("range overflow: " ^ bad) (Result.is_error (Semver.range bad)))
    [ ">9007199254740992"; ">4611686018427387903"; ">18446744073709551616" ];
  List.iter
    (fun bad ->
      check ("version overflow: " ^ bad) (Result.is_error (Semver.version bad)))
    [ "9007199254740992.0.0"; "18446744073709551616.0.0" ]

let () =
  test "semver" semver_cases;
  finish ()
