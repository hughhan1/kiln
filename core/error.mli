(** Failures shared by planning and source access. Payloads describe the failing
    field, resource, or operation; they are not serialized errors from another
    category. *)

type t =
  | Metadata of string * string
  | Unsupported of string
  | Conflict of string
  | Workspace_escape of string
  | Containment of string
  | Missing_start
  | No_version of {
      tool : string;
      requirements : string;
      supported : string list;
    }
  | Filesystem of string
  | Internal of string
  | Cancelled

val show : t -> string
(** Human-readable diagnostic for CLI, JSON logs, and the C API. *)

val exit_status : t -> int
(** CLI status: 130 for cancellation, 1 otherwise. *)

val native_status : t -> int
(** C SDK status: 3 for cancellation, 2 for internal errors, 1 otherwise. *)
