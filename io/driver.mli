(** In-process JSON API. Source paths must be absolute existing directories.
    Expected failures are returned as results; unexpected exceptions remain
    visible to the host boundary. *)

val prepare :
  ?providers:Kiln_planning.Provider.t list ->
  ?cancelled:(unit -> bool) ->
  string ->
  (Kiln_planning.Planner.prepared, Kiln_core.Error.t) result
(** Prepare from the source tree and the pinned toolchains alone, with optional
    providers and cancellation. No external runtime executable is used for
    detection. *)

val plan :
  ?providers:Kiln_planning.Provider.t list ->
  ?cancelled:(unit -> bool) ->
  string ->
  (string, Kiln_core.Error.t) result
(** Like [prepare], returning JSON for the CLI/C API contract. *)
