(** Containment checks for Python dependency references. *)

type requirements = { included : string list; local : bool }
(** Requirements files read from requirements.txt, in include order, and whether
    any line can depend on source beyond those files. *)

val requirements :
  Kiln_planning.Source.t -> (requirements, Kiln_core.Error.t) result
(** Follow requirements/constraint includes, rejecting canonical-path cycles and
    escaping local references. Editable and find-links options, local paths, and
    includes outside the snapshot listing are local. *)

val project :
  Kiln_planning.Source.t ->
  string ->
  Otoml.t ->
  (bool, Kiln_core.Error.t) result
(** Check pyproject dependencies, uv sources/workspaces, and build-backend
    paths; return whether any of them names source in the snapshot. *)

val reference :
  ?project_root:bool ->
  Kiln_planning.Source.t ->
  from:string ->
  string ->
  (bool, Kiln_core.Error.t) result
(** Check a dependency string for a local path; leave remote/package
    dependencies to the installer. Return whether it is a local path. *)
