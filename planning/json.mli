(** JSON boundary decoders. Malformed values become field-labelled metadata
    errors rather than exceptions. *)

type 'a decoder = string -> Yojson.Basic.t -> ('a, Kiln_core.Error.t) result

val metadata : string -> string -> ('a, Kiln_core.Error.t) result
(** Construct a metadata error for a field and diagnostic. *)

val decode : string -> 'a decoder -> string -> ('a, Kiln_core.Error.t) result
(** Parse JSON text, then run the supplied decoder with the given field label.
*)

val string : string decoder
(** Require a JSON string. *)

val boolean : bool decoder
(** Require a JSON boolean. *)

val object_ : (string * Yojson.Basic.t) list decoder
(** Require a JSON object, preserving field order. *)

val list : 'a decoder -> 'a list decoder
(** Decode array elements in order, stopping at the first error. *)

val optional : 'a decoder -> 'a option decoder
(** Treat JSON null as absence; decode all other values. *)

val default : 'a -> 'a decoder -> 'a decoder
(** Use the fallback only for JSON null, not malformed values. *)

val text : (string -> ('a, Kiln_core.Error.t) result) -> 'a decoder
(** Decode a JSON string before applying a domain parser. *)

val member : string -> (string * Yojson.Basic.t) list -> Yojson.Basic.t
(** Look up the first matching field, returning JSON null when absent. *)

val field :
  string ->
  'a decoder ->
  (string * Yojson.Basic.t) list ->
  ('a, Kiln_core.Error.t) result
(** Decode a named field; missing fields reach the decoder as JSON null. *)

val assoc : 'a decoder -> (string * 'a) list decoder
(** Decode every value in an object, preserving keys and order. *)

val string_array : string list -> Yojson.Basic.t
(** Encode strings without interpreting their contents. *)

val encode : Yojson.Basic.t -> string
(** Compact JSON serialization. *)
