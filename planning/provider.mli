(** Provider extension contract: detect an application in a snapshot, then
    prepare its plan from the pinned toolchains. *)

type detection = No_match | Fallback | Explicit

type steps = {
  inputs : string list;
  install : Build_plan.argv list;
  build : Build_plan.argv list;
}
(** What a development environment runs in the source directory: [install]
    whenever a file in [inputs] changes, then [build]. *)

module type S = sig
  val id : string
  (** Stable implementation identifier. *)

  val public_name : string
  (** Public provider name used for configuration. Multiple implementations may
      share a name. *)

  val exclude : string list
  (** Directory/file basenames omitted throughout the source snapshot, besides
      [Source.omitted]. *)

  val includes : string list
  (** Public names of providers whose part of a source this provider builds
      itself, such as Node for a page; they yield to it in detection. *)

  val detect : Source.t -> (detection, Error.t) result
  (** Classify source evidence; explicit matches take precedence over fallbacks.
  *)

  val prepare : Request.t -> Source.t -> (Build_plan.t, Error.t) result
  (** Parse manifests and request overrides, select immutable toolchain pins,
      and generate a validated plan, failing in that order. *)

  val develop : Source.t -> (steps, Error.t) result
  (** Read the manifests a development environment installs from, and nothing
      that configures an image. *)
end

type t = (module S)

val select : t list -> Source.t -> (t, Error.t) result
(** Detect every provider, prefer explicit evidence over fallback evidence, and
    drop candidates another candidate includes. Require a unique candidate and
    propagate malformed metadata. *)
