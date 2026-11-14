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
  Provider.selection -> Request.t -> Source.t -> (prepared, Error.t) result
(** Prepare the selected provider's plan, then append the request's runtime
    variables and PATH additions to its final stage. Fail with [Missing_start]
    when the request requires a startup the plan lacks. *)

val develop :
  Provider.selection -> Request.t -> Source.t -> (string, Error.t) result
(** The JSON of what a development environment runs for the app: [inputs], the
    source paths its install reads, where ["."] is the whole source; [install]
    and [build], arrays of argument vectors; and [start], one or null. Fail when
    the provider has no development steps for the app, or they include a PATH,
    COPY or FILE operation, which only an image runs. *)

val encode : prepared -> string
(** Serialize the Dockerfile together with preparation metadata. *)

val warnings : prepared -> string list
(** Human-readable preparation warnings, also emitted in JSON metadata. *)

val info : prepared -> (string * Yojson.Basic.t) list
(** Success, detected public providers, resolved package versions, and the
    selected toolchain. *)

val failure : Error.t -> (string * Yojson.Basic.t) list
(** Failure metadata with one human-readable error log. *)

val dockerfile : prepared -> string
