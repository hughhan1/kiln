(** SemVer requirements resolved against digest-pinned runtime images. *)

type requirements
type t

val requirements :
  Kiln_planning.Package.requirement list ->
  (requirements, Kiln_core.Error.t) result
(** Parse and canonicalize the set of declarations before discovery. *)

val fingerprint : requirements -> string
(** Stable encoding of the declared constraints for resolution reuse. *)

val image : t -> Kiln_planning.Build_plan.image
(** The immutable image selected for this runtime. *)

val resolve :
  fetch:Kiln_planning.Registry.fetch ->
  repository:string ->
  platforms:Kiln_planning.Platforms.t ->
  accept:(Kiln_core.Semver.version -> bool) ->
  requirements ->
  (t, Kiln_core.Error.t) result
(** Select the greatest stable version satisfying all requirements and [accept],
    and verify every requested platform. *)

val encode : t -> Yojson.Basic.t
(** Serialize version and image pin for reuse. *)

val decode : Yojson.Basic.t -> (t, Kiln_core.Error.t) result
(** Validate a serialized version and image pin. *)

val version : t -> string
(** Canonical resolved version text. *)

val package : string -> requirements -> t -> Kiln_planning.Package.t
(** Report the selected version with its original declarations. *)
