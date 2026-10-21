(** In-process JSON API. Source paths must be absolute existing directories.
    Expected failures are returned as results; unexpected exceptions remain
    visible to the host boundary. *)

type discovery = string -> string option -> (string, Kiln_core.Error.t) result

val prepare_with :
  ?providers:Kiln_planning.Provider.t list ->
  ?cancelled:(unit -> bool) ->
  today:string ->
  fetch:discovery ->
  string ->
  (Kiln_planning.Planner.prepared, Kiln_core.Error.t) result
(** Prepare with injected discovery, date, optional providers, and cancellation.
    No external runtime executable is used for detection. *)

val plan_with :
  ?providers:Kiln_planning.Provider.t list ->
  ?cancelled:(unit -> bool) ->
  today:string ->
  fetch:discovery ->
  string ->
  (string, Kiln_core.Error.t) result
(** Like [prepare_with], returning the serialized Dockerfile and metadata. *)

val prepare :
  ?cancelled:(unit -> bool) ->
  string ->
  (Kiln_planning.Planner.prepared, Kiln_core.Error.t) result
(** Prepare using the current UTC date and live HTTP discovery. *)

val plan :
  ?cancelled:(unit -> bool) -> string -> (string, Kiln_core.Error.t) result
(** Like [prepare], returning JSON for the CLI/C API contract. *)
