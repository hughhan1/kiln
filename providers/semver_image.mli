(** SemVer requirements resolved against digest-pinned runtime images. *)

type requirements
type t

val requirements :
  Kiln_planning.Package.requirement list ->
  (requirements, Kiln_core.Error.t) result
(** Parse and canonicalize the set of declarations before selection. *)

val image : t -> Kiln_planning.Build_plan.image
(** The immutable image selected for this runtime. *)

val resolve :
  tool:string ->
  ?accept:(Kiln_core.Semver.version -> bool) ->
  pins:Pins.image list ->
  requirements ->
  (t, Kiln_core.Error.t) result
(** Select the greatest pin satisfying all requirements and [accept]. *)

val encode : t -> Yojson.Basic.t
(** Serialize version and image pin. *)

val version : t -> string
(** Canonical resolved version text. *)

val package : string -> requirements -> t -> Kiln_planning.Package.t
(** Report the selected version with its original declarations. *)
