type t = Amd64 | Arm64 | Both

let of_strings values =
  let values =
    List.map
      (function "linux/arm64/v8" -> "linux/arm64" | value -> value)
      values
  in
  match List.sort_uniq String.compare values with
  | [ "linux/amd64" ] -> Ok Amd64
  | [ "linux/arm64" ] -> Ok Arm64
  | [ "linux/amd64"; "linux/arm64" ] -> Ok Both
  | [] | _ :: _ ->
      Error
        (Kiln_core.Error.Metadata
           ("platforms", "must contain linux/amd64 and/or linux/arm64"))

let to_strings = function
  | Amd64 -> [ "linux/amd64" ]
  | Arm64 -> [ "linux/arm64" ]
  | Both -> [ "linux/amd64"; "linux/arm64" ]
