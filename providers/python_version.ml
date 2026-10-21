open Result.Syntax
module Error = Kiln_core.Error

type version = int * int * int
type comparison = Eq | Ne | Lt | Le | Gt | Ge
type operator = Comparison of comparison | Compatible
type partial = Major of int | Minor of int * int | Exact of version

type requirement =
  | Compare of comparison * version
  | Include of partial
  | Exclude of partial

let minimum = function
  | Major a -> (a, 0, 0)
  | Minor (a, b) -> (a, b, 0)
  | Exact version -> version

let components text =
  let values = String.split_on_char '.' (String.trim text) in
  let parse part =
    if
      part <> ""
      && String.for_all (function '0' .. '9' -> true | _ -> false) part
    then int_of_string_opt part
    else None
  in
  match List.map parse values with
  | [ Some a ] -> Ok (Major a)
  | [ Some a; Some b ] -> Ok (Minor (a, b))
  | [ Some a; Some b; Some c ] -> Ok (Exact (a, b, c))
  | [] | _ :: _ ->
      Error
        (Error.Metadata
           ( "release " ^ text,
             "expected one to three numeric release components; prerelease and \
              implementation selectors are not supported" ))

let version text =
  let* value = components text in
  match value with
  | Exact version -> Ok version
  | Major _ | Minor _ ->
      Error
        (Error.Metadata ("version " ^ text, "expected a complete stable release"))

let show (a, b, c) = Printf.sprintf "%d.%d.%d" a b c
let compare (a : version) b = Stdlib.compare a b

let clause text =
  let text = String.trim text in
  let operators =
    [
      ("~=", Compatible);
      ("==", Comparison Eq);
      ("!=", Comparison Ne);
      ("<=", Comparison Le);
      (">=", Comparison Ge);
      ("<", Comparison Lt);
      (">", Comparison Gt);
    ]
  in
  match
    List.find_opt (fun (prefix, _) -> String.starts_with ~prefix text) operators
  with
  | None ->
      Error
        (Error.Metadata
           ( "version constraint " ^ text,
             "expected a PEP 440 comparison such as >=3.11,<3.14" ))
  | Some (prefix, op) -> (
      let value = String.trim (String.drop_first (String.length prefix) text) in
      let wildcard = String.ends_with ~suffix:".*" value in
      let* partial =
        components (if wildcard then String.drop_last 2 value else value)
      in
      match (op, wildcard) with
      | Comparison Eq, true -> Ok [ Include partial ]
      | Comparison Ne, true -> Ok [ Exclude partial ]
      | (Comparison (Lt | Le | Gt | Ge) | Compatible), true ->
          Error
            (Error.Metadata
               ("version constraint " ^ text, "wildcards require == or !="))
      | Compatible, false -> (
          match partial with
          | Major _ ->
              Error
                (Error.Metadata
                   ( "version constraint " ^ text,
                     "~= requires at least two release components" ))
          | Minor (a, _) ->
              Ok [ Compare (Ge, minimum partial); Include (Major a) ]
          | Exact (a, b, c) ->
              Ok [ Compare (Ge, (a, b, c)); Include (Minor (a, b)) ])
      | Comparison op, false -> Ok [ Compare (op, minimum partial) ])

let specifier text =
  if String.trim text = "" then Ok []
  else
    let+ clauses =
      Kiln_planning.Support.traverse clause (String.split_on_char ',' text)
    in
    List.concat clauses

let version_file text =
  let+ partial = components text in
  match partial with
  | Exact version -> [ Compare (Eq, version) ]
  | Major _ | Minor _ -> [ Include partial ]

let has_prefix (a, b, c) = function
  | Major major -> a = major
  | Minor (major, minor) -> (a, b) = (major, minor)
  | Exact version -> (a, b, c) = version

let matches requirements version =
  List.for_all
    (function
      | Include prefix -> has_prefix version prefix
      | Exclude prefix -> not (has_prefix version prefix)
      | Compare (op, requested) -> (
          let order = compare version requested in
          match op with
          | Eq -> order = 0
          | Ne -> order <> 0
          | Lt -> order < 0
          | Le -> order <= 0
          | Gt -> order > 0
          | Ge -> order >= 0))
    requirements

let exact requirements =
  List.find_map
    (function
      | Compare (Eq, version) -> Some version
      | Compare ((Ne | Lt | Le | Gt | Ge), _) | Include _ | Exclude _ -> None)
    requirements
