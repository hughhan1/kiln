(** In-process API. Source paths must be absolute existing directories. Expected
    failures are returned as results; unexpected exceptions remain visible to
    the host boundary. *)

val prepare :
  ?providers:Kiln_planning.Provider.t list ->
  ?cancelled:(unit -> bool) ->
  (string * Yojson.Basic.t) list ->
  (Kiln_planning.Build_plan.t, Kiln_planning.Error.t) result
(** Prepare the request object's fields from the source tree and the pinned
    toolchains alone, with optional providers and cancellation. No external
    runtime executable is used for detection. *)

val plan :
  ?providers:Kiln_planning.Provider.t list ->
  ?cancelled:(unit -> bool) ->
  string ->
  (string, Kiln_planning.Error.t) result
(** Parse JSON request text and return the C API's JSON response: the image
    plan, or for a [development] request, which names only its [source] and
    reads no configuration, {!Kiln_planning.Planner.develop}'s steps. *)
