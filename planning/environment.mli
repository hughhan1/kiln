(** Validated application build variables. Values may contain newlines but not
    NUL; names contain only alphanumeric characters and underscores. *)

type t

module Names : sig
  type t

  val empty : t
  val elements : t -> string list
  val is_empty : t -> bool
  val union : t -> t -> t

  val decode : t Json.decoder
  (** Parse valid, unique secret names. *)
end

val bindings : t -> (string * string) list
(** Values sorted by name, with later assignments replacing earlier ones. *)

val names : t -> Names.t
val valid_name : string -> bool
val decode : t Json.decoder

val of_args :
  getenv:(string -> string option) ->
  string list ->
  (t, Kiln_core.Error.t) result
(** Parse assignments and inherited names; later values win. Missing inherited
    variables and empty assignments are skipped, but names must always be valid.
*)

val config : t -> string -> string option
(** A trimmed, nonblank [RAILPACK_] setting, addressed without the prefix. *)

val encode : t -> Yojson.Basic.t
