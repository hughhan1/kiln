open Result.Syntax

type t = {
  files : string list;
  links : bool;
  read : string -> (string, Kiln_core.Error.t) result;
  resolve_reference :
    from:string -> string -> (string, Kiln_core.Error.t) result;
}

let check_reference source ~from reference =
  let+ _ = source.resolve_reference ~from reference in
  ()

let has source path = List.mem path source.files

let copy ~exclude paths destination =
  Build_plan.context
    ~exclude:
      (List.concat_map (fun name -> [ name; "**/" ^ name ]) (".git" :: exclude))
    paths destination

let version source path =
  if not (has source path) then Ok None
  else
    let* bytes = source.read path in
    let value =
      String.trim
        (String.concat "\n"
           (List.map
              (String.take_first_while (( <> ) '#'))
              (String.split_on_char '\n' bytes)))
    in
    if value = "" then
      Error (Kiln_core.Error.Metadata (path, "must not be empty"))
    else Ok (Some value)

let workspace patterns =
  match
    List.find_opt
      (fun pattern ->
        let path = String.drop_first_while (( = ) '!') pattern in
        String.starts_with ~prefix:"/" path
        || List.mem ".." (String.split_on_char '/' path))
      patterns
  with
  | None -> Ok ()
  | Some pattern -> Error (Kiln_core.Error.Workspace_escape pattern)
