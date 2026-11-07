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

let select ~parse ~compare ~accept ~none rows =
  let* candidates =
    traverse
      (fun row ->
        let+ version = parse row in
        (version, row))
      rows
  in
  Option.to_result ~none
    (maximum
       ~compare:(fun (a, _) (b, _) -> compare a b)
       (List.filter (fun (version, row) -> accept version row) candidates))

let inside root path =
  path = root || String.starts_with ~prefix:(Filename.concat root "") path
