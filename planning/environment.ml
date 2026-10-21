open Result.Syntax
module Variables = Map.Make (String)

type t = string Variables.t

let bindings = Variables.bindings

let valid_name name =
  name <> ""
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false)
       name

let check_name label name =
  if valid_name name then Ok () else Json.metadata label "invalid variable name"

module Names = struct
  include Set.Make (String)

  let decode label value =
    let* names = Json.default [] (Json.list Json.string) label value in
    let+ () = Support.iter (check_name label) names in
    of_list names
end

let names env = Names.of_list (List.map fst (bindings env))

let of_pairs label pairs =
  let check (name, value) =
    let* () = check_name label name in
    if String.contains value '\000' then
      Json.metadata label "value must not contain NUL"
    else Ok ()
  in
  let+ () = Support.iter check pairs in
  Variables.of_list pairs

let decode label value =
  let* pairs = Json.default [] (Json.assoc Json.string) label value in
  of_pairs label pairs

let of_args ~getenv entries =
  let entry text =
    let assignment = String.split_first ~sep:"=" text in
    let name = Option.value ~default:text (Option.map fst assignment) in
    let+ () = check_name "environment" name in
    let value =
      match assignment with
      | Some (_, "") -> None
      | Some (_, value) -> Some value
      | None -> getenv name
    in
    Option.map (fun value -> (name, value)) value
  in
  let* pairs = Support.traverse entry entries in
  of_pairs "environment" (List.filter_map Fun.id pairs)

let config env name =
  match Variables.find_opt ("RAILPACK_" ^ name) env with
  | None -> None
  | Some value -> (
      match String.trim value with "" -> None | value -> Some value)

let encode env =
  `Assoc (List.map (fun (key, value) -> (key, `String value)) (bindings env))
