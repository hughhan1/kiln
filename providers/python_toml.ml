open Result.Syntax
module Error = Kiln_planning.Error

let read source path =
  let* bytes = source.Kiln_planning.Source.read path in
  Result.map_error
    (fun message -> Error.Metadata (path, message))
    (Otoml.Parser.from_string_result bytes)

let field document path decode =
  try Ok (Some (Otoml.find document decode path)) with
  | Otoml.Key_error _ -> Ok None
  | Otoml.Type_error message ->
      Error (Error.Metadata (String.concat "." path, message))

let string document path = field document path (Otoml.get_string ~strict:true)

let default fallback document path decode =
  let+ value = field document path decode in
  Option.value ~default:fallback value

let array document path decode =
  default [] document path (Otoml.get_array decode)

let strings document path = array document path (Otoml.get_string ~strict:true)

let table document path decode =
  default [] document path (Otoml.get_table_values decode)

let read_optional source path =
  if Kiln_planning.Source.has source path then read source path
  else Ok (Otoml.TomlTable [])

let rec keyed key = function
  | Otoml.TomlTable fields | Otoml.TomlInlineTable fields ->
      List.concat_map (fun (name, value) -> keyed name value) fields
  | Otoml.TomlTableArray values | Otoml.TomlArray values ->
      List.concat_map (keyed key) values
  | Otoml.TomlString text -> [ (key, text) ]
  | Otoml.TomlInteger _ | Otoml.TomlFloat _ | Otoml.TomlBoolean _
  | Otoml.TomlOffsetDateTime _ | Otoml.TomlLocalDateTime _
  | Otoml.TomlLocalDate _ | Otoml.TomlLocalTime _ ->
      []

let keyed_strings document = keyed "" document
