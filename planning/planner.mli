(** Preparation results and the C API's response schema. *)

type prepared = private { plan : Build_plan.t; resolution : Yojson.Basic.t }

val prepare : Provider.t -> Request.t -> Source.t -> (prepared, Error.t) result
(** Prepare the selected provider's plan, then append the configured PATH
    additions to its final stage. *)

val develop : Provider.t -> Source.t -> (string, Error.t) result
(** The JSON of what a development environment runs in the source: [install],
    then [build], each an array of argument vectors. *)

val encode : prepared -> string
(** The JSON of the plan: its [dockerfile]. *)

val failure : Error.t -> (string * Yojson.Basic.t) list
(** The JSON members of a failure: its [error] message. *)

val dockerfile : prepared -> string
