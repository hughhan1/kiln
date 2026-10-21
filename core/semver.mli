(** npm-style semantic versions and ranges, parsed before comparison. *)

type version
type range

val version : string -> (version, string) result
(** Parse a complete version, optionally prefixed with [v]. Reject malformed
    components and core integers outside npm's safe range. *)

val range : string -> (range, string) result
(** Parse npm comparisons, wildcards, caret, tilde, hyphen, and disjunction
    ranges. *)

val show : version -> string
(** Canonical version text, retaining prerelease and build identifiers. *)

val major : version -> int
(** The major release component. *)

val stable : version -> bool
(** Whether the version has no prerelease identifiers. *)

val compare : version -> version -> int
(** SemVer precedence; build metadata does not affect ordering. *)

val matches : range -> version -> bool
(** Range membership, including npm's exclusion of unmentioned prerelease lines.
*)
