(** A contained, read-only snapshot. [files] are source-relative names; [links]
    records symlink presence. [read] accepts contained relative or absolute
    paths. [resolve_reference] resolves relative to the manifest named by
    [from], returning a canonical absolute path and rejecting missing, excluded,
    or escaping targets. *)

type t = {
  files : string list;
  links : bool;
  read : string -> (string, Kiln_core.Error.t) result;
  resolve_reference :
    from:string -> string -> (string, Kiln_core.Error.t) result;
}

val has : t -> string -> bool
(** Whether a source-relative filename is in the snapshot. *)

val check_reference :
  t -> from:string -> string -> (unit, Kiln_core.Error.t) result
(** Require a contained dependency target without retaining its canonical path.
*)

val version : t -> string -> (string option, Kiln_core.Error.t) result
(** Read an optional version file, removing comments and surrounding whitespace.
    Reject an empty present file. *)

val workspace : string list -> (unit, Kiln_core.Error.t) result
(** Reject absolute workspace patterns and parent-directory components,
    including negated patterns. *)

val copy : exclude:string list -> string list -> string -> Build_plan.operation
(** Copy snapshot paths while excluding .git and provider exclusions at every
    depth. *)

val manifests : string list -> Build_plan.operation list
(** Copy listed snapshot files to their own paths under the working directory,
    one COPY per directory, so dependency installation can precede the full
    source copy. *)
