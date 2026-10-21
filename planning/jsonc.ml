open Angstrom

let whitespace = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let tokens =
  let space =
    skip_many
      (satisfy whitespace *> skip_while whitespace
      <|> string "//" *> skip_while (fun c -> c <> '\n' && c <> '\r')
      <|> string "/*" *> many_till any_char (string "*/") *> return ())
  in
  let quoted =
    consumed
      (char '"'
       *> skip_many
            (char '\\' *> any_char <|> satisfy (fun c -> c <> '"' && c <> '\\'))
      <* char '"')
  in
  let punctuation = "[]{}:,/" in
  let token =
    quoted
    <|> (satisfy (String.contains punctuation) >>| String.make 1)
    <|> take_while1 (fun c ->
        not (whitespace c || c = '"' || String.contains punctuation c))
  in
  space *> many (token <* space) <* end_of_input

type position = After_value | After_separator

let rec trailing_commas position = function
  | "," :: (("]" | "}") :: _ as rest) when position = After_value ->
      trailing_commas position rest
  | token :: rest ->
      let position =
        if List.mem token [ "["; "{"; ","; ":" ] then After_separator
        else After_value
      in
      token :: trailing_commas position rest
  | [] -> []

let object_ label text =
  match parse_string ~consume:Consume.All tokens text with
  | Error message -> Json.metadata label message
  | Ok tokens ->
      Json.decode label Json.object_
        (String.concat " " (trailing_commas After_separator tokens))
