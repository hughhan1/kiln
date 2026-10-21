(** Failures shared by planning, discovery, and source access. Payloads describe
    the failing field, resource, or operation; they are not serialized errors
    from another category. *)

type t =
  | Metadata of string * string
  | Unsupported of string
  | Conflict of string
  | Workspace_escape of string
  | Containment of string
  | Missing_start
  | No_version of { tool : string; requirements : string }
  | Missing_metadata of string
  | Target_unavailable of string
  | Integrity_mismatch
  | Invalid_resolution
  | Filesystem of string
  | Discovery of string
  | Transient of string
  | Internal of string
  | Cancelled

val show : t -> string
(** Human-readable diagnostic for CLI, JSON logs, and the C API. *)

val exit_status : t -> int
(** CLI status: 130 for cancellation, 75 for transient discovery, 1 otherwise.
*)

val native_status : t -> int
(** C SDK status: 3 for cancellation, 75 for transient discovery, 2 for internal
    errors, 1 otherwise. *)
