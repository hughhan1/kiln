open Result.Syntax

let traverse f values =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs ->
        let* y = f x in
        loop (y :: acc) xs
  in
  loop [] values

let rec iter f = function
  | [] -> Ok ()
  | x :: xs ->
      let* _ = f x in
      iter f xs

let maximum ~compare =
  List.fold_left
    (fun best next ->
      match best with
      | Some previous when compare previous next >= 0 -> best
      | None | Some _ -> Some next)
    None

let inside root path =
  path = root || String.starts_with ~prefix:(Filename.concat root "") path
