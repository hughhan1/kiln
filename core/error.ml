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

let show = function
  | Metadata (field, message) -> field ^ ": " ^ message
  | Unsupported message -> "unsupported: " ^ message
  | Conflict message -> "conflicting " ^ message
  | Workspace_escape pattern -> "workspace pattern escapes source: " ^ pattern
  | Containment path -> "path escapes or is missing from snapshot: " ^ path
  | Missing_start ->
      "no startup: provide start argv or an application entry point"
  | No_version { tool; requirements; supported } ->
      "no compatible " ^ tool ^ " version for " ^ requirements ^ "; supported: "
      ^ String.concat ", " supported
  | Filesystem message -> "filesystem: " ^ message
  | Internal message -> "internal error: " ^ message
  | Cancelled -> "operation cancelled"

let statuses = function
  | Cancelled -> (130, 3)
  | Internal _ -> (1, 2)
  | Metadata _ | Unsupported _ | Conflict _ | Workspace_escape _ | Containment _
  | Missing_start | No_version _ | Filesystem _ ->
      (1, 1)

let exit_status error = fst (statuses error)
let native_status error = snd (statuses error)
