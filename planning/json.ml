type 'a decoder = string -> Yojson.Basic.t -> ('a, Kiln_core.Error.t) result

open Result.Syntax

let metadata field message = Error (Kiln_core.Error.Metadata (field, message))

let decode field parse text =
  let* value =
    try Ok (Yojson.Basic.from_string text)
    with Yojson.Json_error message -> metadata field message
  in
  parse field value

let string field = function
  | `String value -> Ok value
  | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ ->
      metadata field "expected string"

let boolean field = function
  | `Bool value -> Ok value
  | `Null | `Int _ | `Float _ | `String _ | `List _ | `Assoc _ ->
      metadata field "expected boolean"

let object_ field = function
  | `Assoc values -> Ok values
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
      metadata field "expected object"

let list parse field = function
  | `List values -> Support.traverse (parse field) values
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _ ->
      metadata field "expected array"

let optional parse field = function
  | `Null -> Ok None
  | (`Bool _ | `Int _ | `Float _ | `String _ | `List _ | `Assoc _) as value ->
      let+ parsed = parse field value in
      Some parsed

let default value parse field json =
  let+ parsed = optional parse field json in
  Option.value ~default:value parsed

let text parse field value =
  let* value = string field value in
  parse value

let member name fields =
  Option.value ~default:`Null (List.assoc_opt name fields)

let field name parse fields = parse name (member name fields)

let assoc parse name value =
  let* fields = object_ name value in
  Support.traverse
    (fun (key, value) ->
      let+ parsed = parse name value in
      (key, parsed))
    fields

let string_array values = `List (List.map (fun value -> `String value) values)
let encode value = Yojson.Basic.to_string value
