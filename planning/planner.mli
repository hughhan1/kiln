(** Preparation results and the shared CLI/library response schema. *)

type prepared = private {
  plan : Build_plan.t;
  secrets : Environment.Names.t;
  resolution : Yojson.Basic.t;
  detected : string list;
  packages : Package.t list;
  metadata : (string * string) list;
}

val prepare :
  Provider.selection ->
  today:string ->
  fetch:Registry.fetch ->
  Request.t ->
  Source.t ->
  (prepared, Kiln_core.Error.t) result
(** Load a provider application, reuse a compatible resolution or discover one,
    then generate the plan. [today] is an ISO calendar date. *)

val encode : prepared -> string
(** Serialize the Dockerfile together with preparation metadata. *)

val warnings : prepared -> string list
(** Human-readable preparation warnings, also emitted in JSON metadata. *)

val info : prepared -> (string * Yojson.Basic.t) list
(** Success, detected public providers, resolved package versions, and reusable
    resolution fields. *)

val failure : Kiln_core.Error.t -> (string * Yojson.Basic.t) list
(** Failure metadata with one human-readable error log. *)

val dockerfile : prepared -> string
