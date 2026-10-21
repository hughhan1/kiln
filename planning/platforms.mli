(** Nonempty sets of the Linux targets supported by Kiln. *)
type t = Amd64 | Arm64 | Both

val of_strings : string list -> (t, Kiln_core.Error.t) result
(** Reject empty or unsupported selections; repeated targets are accepted.
    [linux/arm64/v8] is normalized to [linux/arm64]. *)

val to_strings : t -> string list
(** Canonical order for registry checks, fingerprints, and Buildx arguments. *)
