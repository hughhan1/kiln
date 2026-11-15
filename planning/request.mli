(** Validated preparation inputs: the request's source and kind. *)

type run = {
  command : Build_plan.argv;
  mounts : (Build_plan.input * string) list;
      (** Build contexts, by name, mounted read-only at these paths. *)
  overlay : Build_plan.input option;
      (** A build context, by name, copied over the app before the command. *)
  packages : string list;
      (** Python requirements the command can import, installed apart from the
          app's own. *)
}
(** A command run with no network in the app's image, without its build, whose
    stdout, stderr and exit status the plan exports as [/stdout], [/stderr] and
    [/status]. *)

type kind = Image of { start : Build_plan.argv option } | Run of run
type t = private { source : string; kind : kind }

val development : (string * Yojson.Basic.t) list -> bool
(** Whether the JSON request is a development request: an empty [development]
    object and no other kind. *)

val decode : (string * Yojson.Basic.t) list -> (t, Error.t) result
(** Decode the JSON request's [source] and its one kind: an [image] object with
    an optional [start] argument vector, or a [run] object. A request naming no
    kind plans an image. *)

val start : t -> Build_plan.argv option
(** The startup an image request names. *)

val builds : t -> bool
(** Whether the plan builds the app, as an image does and a run does not. *)
