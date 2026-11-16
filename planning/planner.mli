(** The C API's responses: image plans, development plans and failures. *)

val prepare :
  Provider.t -> Request.t -> Source.t -> (Build_plan.t, Error.t) result
(** Prepare the selected provider's plan, then append the configured PATH
    additions to its final stage. *)

val develop :
  Provider.t -> Source.t -> Build_plan.argv -> (string, Error.t) result
(** The JSON of the plan a development environment follows in the source:
    [install], whose [commands] rerun whenever its [inputs] digest of the files
    they install from changes; then the [prepare] commands and the [start]
    argument vector. *)

val encode : Build_plan.t -> string
(** The JSON of the plan: its [dockerfile]. *)

val failure : Error.t -> string
(** The JSON of a failure: its [error] message. *)
