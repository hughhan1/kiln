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
