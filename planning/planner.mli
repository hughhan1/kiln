(** The C API's responses: image plans, development steps and failures. *)

val prepare :
  Provider.t -> Request.t -> Source.t -> (Build_plan.t, Error.t) result
(** Prepare the selected provider's plan, then append the configured PATH
    additions to its final stage. *)

val develop : Provider.t -> Source.t -> (string, Error.t) result
(** The JSON of what a development environment runs in the source: [install],
    then [build], each an array of argument vectors. *)

val encode : Build_plan.t -> string
(** The JSON of the plan: its [dockerfile]. *)

val failure : Error.t -> string
(** The JSON of a failure: its [error] message. *)
