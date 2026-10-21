(** Containment checks for Python dependency references. *)

val requirements : Kiln_planning.Source.t -> (unit, Kiln_core.Error.t) result
(** Follow requirements/constraint includes, rejecting canonical-path cycles and
    escaping local references. *)

val project :
  Kiln_planning.Source.t ->
  string ->
  Otoml.t ->
  (unit, Kiln_core.Error.t) result
(** Check pyproject dependencies, uv sources/workspaces, and build-backend
    paths. *)

val reference :
  ?project_root:bool ->
  Kiln_planning.Source.t ->
  from:string ->
  string ->
  (unit, Kiln_core.Error.t) result
(** Check a dependency string for a local path; leave remote/package
    dependencies to the installer. *)
