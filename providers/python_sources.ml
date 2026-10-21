module Json = Kiln_planning.Json
module Source = Kiln_planning.Source
module Support = Kiln_planning.Support
open Result.Syntax
module Toml = Python_toml

let relative from path =
  if Filename.dirname from = "." then path
  else Filename.concat (Filename.dirname from) path

let reference ?(project_root = false) source ~from text =
  let text = String.trim text in
  let text =
    match String.split_first ~sep:"@" text with
    | Some (_, path) -> String.trim path
    | None -> text
  in
  let file_url = String.starts_with ~prefix:"file:" text in
  let text = String.trim (String.take_first_while (( <> ) ';') text) in
  let text =
    if String.starts_with ~prefix:"file://" text then String.drop_first 7 text
    else if String.starts_with ~prefix:"file:" text then
      String.drop_first 5 text
    else text
  in
  let project_root_prefixes = [ "${PROJECT_ROOT}/"; "/${PROJECT_ROOT}/" ] in
  let text, project_root_reference =
    if project_root then
      match
        List.find_opt
          (fun prefix -> String.starts_with ~prefix text)
          project_root_prefixes
      with
      | Some prefix -> (String.drop_first (String.length prefix) text, true)
      | None when text = "${PROJECT_ROOT}" || text = "/${PROJECT_ROOT}" ->
          (".", true)
      | None -> (text, false)
    else (text, false)
  in
  let from = if project_root_reference then "pyproject.toml" else from in
  if String.starts_with ~prefix:"~" text then
    Json.metadata from
      "home-relative dependencies are unsupported; use a contained relative \
       path"
  else if
    file_url
    || (String.contains text '/' && not (String.contains text ':'))
    || List.exists (fun prefix -> String.starts_with ~prefix text) [ "."; "/" ]
    || Source.has source (relative from text)
    || List.exists
         (fun file ->
           String.starts_with ~prefix:(relative from (text ^ "/")) file)
         source.Source.files
  then
    if String.contains text '%' || String.contains text '$' then
      Json.metadata from
        "encoded or variable local paths are unsupported; use a plain relative \
         path"
    else Source.check_reference source ~from text
  else Ok ()

type requirement_option = Include_file | Local_dependency

module Paths = Set.Make (String)

let requirements source =
  let rec read seen ~from path =
    let* canonical = source.Source.resolve_reference ~from path in
    if Paths.mem canonical seen then
      Json.metadata path "cyclic requirements include"
    else
      let path =
        if Filename.is_relative path then relative from path else path
      in
      let* bytes = source.Source.read path in
      Support.iter
        (line (Paths.add canonical seen) path)
        (String.split_on_char '\n' bytes)
  and line seen from line =
    let value = String.trim (String.take_first_while (( <> ) '#') line) in
    let options =
      [
        ("--requirement", Include_file);
        ("--constraint", Include_file);
        ("-r", Include_file);
        ("-c", Include_file);
        ("--editable", Local_dependency);
        ("-e", Local_dependency);
        ("--find-links", Local_dependency);
        ("-f", Local_dependency);
      ]
    in
    let found =
      List.find_map
        (fun (option, kind) ->
          if String.starts_with ~prefix:option value then
            Some
              ( kind,
                String.trim
                  (String.drop_first_while (( = ) '=')
                     (String.drop_first (String.length option) value)) )
          else None)
        options
    in
    match found with
    | Some (Include_file, path) -> read seen ~from path
    | Some (Local_dependency, path) -> reference source ~from path
    | None -> reference source ~from value
  in
  if Source.has source "requirements.txt" then
    read Paths.empty ~from:"requirements.txt" "requirements.txt"
  else Ok ()

let project source path document =
  let* dependencies = Toml.strings document [ "project"; "dependencies" ] in
  let* optional =
    Toml.table document
      [ "project"; "optional-dependencies" ]
      (Otoml.get_array (Otoml.get_string ~strict:true))
  in
  let* () =
    Support.iter
      (reference ~project_root:true source ~from:path)
      (dependencies @ List.concat_map snd optional)
  in
  let* sources =
    Toml.table document [ "tool"; "uv"; "sources" ] Otoml.get_value
  in
  let* () =
    Support.iter
      (fun (_, value) ->
        let* local = Toml.string value [ "path" ] in
        match local with
        | None -> Ok ()
        | Some local -> reference source ~from:path local)
      sources
  in
  let* pdm = Toml.field document [ "tool"; "pdm" ] Otoml.get_value in
  let pdm_source_urls =
    let rec collect key = function
      | Otoml.TomlTable values | Otoml.TomlInlineTable values ->
          List.concat_map (fun (name, value) -> collect name value) values
      | Otoml.TomlTableArray values | Otoml.TomlArray values ->
          List.concat_map (collect key) values
      | Otoml.TomlString value when key = "url" -> [ value ]
      | Otoml.TomlString _ | Otoml.TomlInteger _ | Otoml.TomlFloat _
      | Otoml.TomlBoolean _ | Otoml.TomlOffsetDateTime _
      | Otoml.TomlLocalDateTime _ | Otoml.TomlLocalDate _
      | Otoml.TomlLocalTime _ ->
          []
    in
    Option.fold ~none:[] ~some:(collect "") pdm
  in
  let* () =
    Support.iter
      (fun url ->
        if String.starts_with ~prefix:"file:" url || String.contains url '$'
        then reference ~project_root:true source ~from:path url
        else Ok ())
      pdm_source_urls
  in
  let* members =
    Toml.strings document [ "tool"; "uv"; "workspace"; "members" ]
  in
  let* excluded =
    Toml.strings document [ "tool"; "uv"; "workspace"; "exclude" ]
  in
  let* () = Source.workspace (members @ excluded) in
  let* backend = Toml.strings document [ "build-system"; "backend-path" ] in
  Support.iter (Source.check_reference source ~from:path) backend
