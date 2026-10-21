open Result.Syntax

type command = Inherit | Operation of Build_plan.operation
type t = command list

let paths commands =
  List.filter_map
    (function
      | Operation (Build_plan.Prepend_path path) -> Some path
      | Inherit
      | Operation
          ( Build_plan.Run _ | Build_plan.Copy _ | Build_plan.Workdir _
          | Build_plan.Env _ | Build_plan.User _ | Build_plan.Entrypoint _
          | Build_plan.Start _ | Build_plan.Write _ ) ->
          None)
    commands

let expand commands inherited =
  List.concat_map
    (function Inherit -> inherited | Operation command -> [ command ])
    commands

let shell text =
  let+ argv = Build_plan.shell text in
  [ Operation (Build_plan.Run argv) ]

let words text =
  let open Angstrom in
  let whitespace = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false in
  let escaped = char '\\' *> any_char >>| String.make 1 in
  let single = char '\'' *> take_while (( <> ) '\'') <* char '\'' in
  let double =
    char '"' *> many (escaped <|> take_while1 (fun c -> c <> '"' && c <> '\\'))
    <* char '"' >>| String.concat ""
  in
  let bare =
    take_while1 (fun c -> not (whitespace c || List.mem c [ '\''; '"'; '\\' ]))
  in
  let word =
    many1 (single <|> double <|> escaped <|> bare) >>| String.concat ""
  in
  Result.map_error
    (fun message -> Kiln_core.Error.Metadata ("command", message))
    (parse_string ~consume:Consume.All
       (skip_while whitespace *> many (word <* skip_while whitespace))
       text)

let write ~assets ~mode path name =
  let+ contents =
    Option.to_result
      ~none:(Kiln_core.Error.Metadata ("asset", "not found: " ^ name))
      (List.assoc_opt name assets)
  in
  Operation (Build_plan.Write { path; contents; mode })

let copy input src dest =
  Operation
    (Build_plan.Copy
       {
         input;
         paths = [ src ];
         destination = dest;
         exclude = [];
         owner = None;
       })

let operation ~assets fields =
  let open Json in
  let* _ = field "customName" (optional string) fields in
  let keys =
    List.map fst fields
    |> List.filter (( <> ) "customName")
    |> List.sort String.compare
  in
  match keys with
  | [ "cmd" ] -> (
      let* command = field "cmd" string fields in
      match command with
      | "..." | "sh -c '...'" -> Ok Inherit
      | command ->
          let* arguments = words command in
          let+ command = Build_plan.run arguments in
          Operation command)
  | [ "path" ] ->
      let+ path = field "path" string fields in
      Operation (Build_plan.Prepend_path path)
  | [ "name"; "path" ] | [ "mode"; "name"; "path" ] ->
      let* path = field "path" string fields in
      let* name = field "name" string fields in
      let* mode =
        field "mode"
          (default 0o644 (fun label -> function
            | `Int 0 -> Ok 0o644
            | `Int mode -> Ok mode
            | _ -> metadata label "expected integer"))
          fields
      in
      write ~assets ~mode path name
  | [ "dest"; "src" ] | [ "dest"; "image"; "src" ] ->
      let* src = field "src" string fields in
      let* dest = field "dest" string fields in
      let* image = field "image" (default "" string) fields in
      let+ input =
        match image with
        | "" -> Ok Build_plan.Context
        | image -> Build_plan.external_image image
      in
      copy input src dest
  | _ ->
      metadata "command" "expected exactly one RUN, PATH, FILE, or COPY object"

let pair build payload =
  let* parts = words payload in
  match parts with
  | [ first; second ] -> build first second
  | _ -> Json.metadata "command" "expected two COPY or FILE arguments"

let string_command ~assets text =
  let shell text =
    match text with
    | "..." -> Ok Inherit
    | text ->
        let+ argv = Build_plan.shell text in
        Operation (Build_plan.Run argv)
  in
  match String.split_first ~sep:":" text with
  | Some (prefix, payload) -> (
      match String.take_first_while (( <> ) '#') prefix with
      | "RUN" -> shell payload
      | "PATH" -> Ok (Operation (Build_plan.Prepend_path payload))
      | "COPY" ->
          pair (fun src dest -> Ok (copy Build_plan.Context src dest)) payload
      | "FILE" -> pair (write ~assets ~mode:0o644) payload
      | _ -> shell text)
  | None -> shell text

let decode ~assets label value =
  let command label = function
    | `String text -> string_command ~assets text
    | value ->
        let* fields = Json.object_ label value in
        operation ~assets fields
  in
  let* commands = Json.list command label value in
  let operations = expand commands [] in
  let+ () = Build_plan.validate_operations operations in
  commands
