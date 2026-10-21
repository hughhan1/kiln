(** Stable numeric Python/uv releases and the supported PEP 440 subset.
    Prereleases and implementation selectors are rejected. *)

type version
type requirement

val version : string -> (version, Kiln_core.Error.t) result
(** Require three numeric release components. *)

val show : version -> string
(** Canonical three-component release string. *)

val compare : version -> version -> int
(** Numeric release ordering. *)

val specifier : string -> (requirement list, Kiln_core.Error.t) result
(** Parse comma-separated comparisons, equality wildcards, and
    compatible-release constraints; an empty specifier imposes no constraints.
*)

val version_file : string -> (requirement list, Kiln_core.Error.t) result
(** Parse a one-, two-, or three-component version-file selector. Partial
    selectors match a release prefix. *)

val matches : requirement list -> version -> bool
(** Require every constraint to hold for a release. *)

val exact : requirement list -> version option
(** Return an equality-pinned release, if present. Other constraints must still
    be checked with [matches]. *)
