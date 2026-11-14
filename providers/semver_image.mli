(** SemVer requirements resolved against digest-pinned runtime images. *)

type requirements

val requirements : string list -> (requirements, Kiln_planning.Error.t) result
(** Parse and canonicalize the set of declarations before selection. *)

val resolve :
  tool:string ->
  ?accept:(Kiln_planning.Semver.version -> bool) ->
  pins:Pins.image list ->
  requirements ->
  (Kiln_planning.Build_plan.image, Kiln_planning.Error.t) result
(** The image of the greatest pin satisfying all requirements and [accept]. *)
