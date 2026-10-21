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

let show = function
  | Metadata (field, message) -> field ^ ": " ^ message
  | Unsupported message -> "unsupported: " ^ message
  | Conflict message -> "conflicting " ^ message
  | Workspace_escape pattern -> "workspace pattern escapes source: " ^ pattern
  | Containment path -> "path escapes or is missing from snapshot: " ^ path
  | Missing_start ->
      "no startup: provide start argv or an application entry point"
  | No_version { tool; requirements } ->
      "no compatible " ^ tool ^ " version: " ^ requirements
  | Missing_metadata message -> "missing metadata: " ^ message
  | Target_unavailable tag -> "image " ^ tag ^ " lacks a requested target"
  | Integrity_mismatch -> "integrity does not match the registry tarball"
  | Invalid_resolution -> "invalid reused resolution"
  | Filesystem message -> "filesystem: " ^ message
  | Discovery message | Transient message ->
      "toolchain discovery failed: " ^ message
  | Internal message -> "internal error: " ^ message
  | Cancelled -> "operation cancelled"

let statuses = function
  | Cancelled -> (130, 3)
  | Transient _ -> (75, 75)
  | Internal _ -> (1, 2)
  | Metadata _ | Unsupported _ | Conflict _ | Workspace_escape _ | Containment _
  | Missing_start | No_version _ | Missing_metadata _ | Target_unavailable _
  | Integrity_mismatch | Invalid_resolution | Filesystem _ | Discovery _ ->
      (1, 1)

let exit_status error = fst (statuses error)
let native_status error = snd (statuses error)
