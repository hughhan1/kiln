(** JSON objects with comments and trailing commas. *)

val object_ :
  string -> string -> ((string * Yojson.Basic.t) list, Kiln_core.Error.t) result
(** Strip comments and trailing commas outside strings, then decode an object.
    Report malformed input with the supplied label. *)
