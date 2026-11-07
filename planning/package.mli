(** Result metadata for resolved packages, preserving the original declarations.
*)

type requirement = { version : string; source : string }
type t

val require : string -> string -> requirement

val optional : string -> string option -> requirement list
(** Preserve a declaration's text and origin when it is present. *)

val versions : requirement list -> string list

val no_version :
  tool:string -> requirement list -> string list -> Kiln_core.Error.t
(** The failure for declarations that no supported version satisfies. *)

val resolve :
  ?default_source:string -> string -> requirement list -> string -> t
(** Use [default_source] only when no version was requested. *)

val encode : t -> string * Yojson.Basic.t
(** Encode [requestedVersion] only for declared versions. Multiple requests are
    summarized with [ && ]; source names are summarized with [, ]. *)
