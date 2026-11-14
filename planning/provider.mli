(** Provider extension contract: detect an application in a snapshot, then
    prepare its plan from the pinned toolchains. *)

type detection = No_match | Fallback | Explicit

type steps = { install : Build_plan.argv list; build : Build_plan.argv list }
(** What a development environment runs in the source directory: [install], then
    [build]. *)

type output = {
  plan : Build_plan.t;
  packages : Package.t list;
      (** Resolved versions together with the declarations that selected them.
      *)
  metadata : (string * string) list;
  resolution : Yojson.Basic.t;  (** The selected toolchain pins. *)
}

module type S = sig
  val id : string
  (** Stable implementation identifier. *)

  val public_name : string
  (** Public provider name used for requests and detected-provider metadata.
      Multiple implementations may share a name. *)

  val exclude : string list
  (** Directory/file basenames omitted throughout the source snapshot, besides
      [Source.omitted]. *)

  val includes : string list
  (** Public names of providers whose part of a source this provider builds
      itself, such as Node for a page; they yield to it in detection. *)

  val detect : Source.t -> (detection, Error.t) result
  (** Classify source evidence; explicit matches take precedence over fallbacks.
  *)

  val prepare : Request.t -> Source.t -> (output, Error.t) result
  (** Parse manifests and request overrides, select immutable toolchain pins,
      and generate a validated plan, failing in that order. *)

  val develop : Source.t -> (steps, Error.t) result
  (** Read the manifests a development environment installs from, and nothing
      that configures an image. *)
end

type t = (module S)

type selection = { provider : t; detected : string list }
(** Selected implementation and the independently detected public names. *)

val select :
  t list -> requested:string option -> Source.t -> (selection, Error.t) result
(** Detect every provider and prefer explicit evidence over fallback evidence.
    Without a requested name, drop candidates another candidate includes.
    Restrict selection to a requested public name when supplied, allowing an
    undetected implementation if that name has no detection evidence. Require a
    unique candidate and propagate malformed metadata. [detected] always lists
    the strongest public detections independently of the request. *)
