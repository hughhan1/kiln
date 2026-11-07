(** Provider extension contract. Application and resolution types stay private
    to each provider. *)

type detection = No_match | Fallback | Explicit

module type S = sig
  type application
  type resolution

  val id : string
  (** Stable implementation identifier. *)

  val public_name : string
  (** Public provider name used for requests and detected-provider metadata.
      Multiple implementations may share a name. *)

  val exclude : string list
  (** Directory/file basenames omitted throughout the source snapshot. *)

  val detect : Source.t -> (detection, Kiln_core.Error.t) result
  (** Classify source evidence; explicit matches take precedence over fallbacks.
  *)

  val load : Request.t -> Source.t -> (application, Kiln_core.Error.t) result
  (** Parse manifests and request overrides into the provider's application
      type. *)

  val resolve : application -> (resolution, Kiln_core.Error.t) result
  (** Select immutable toolchain pins for the application. *)

  val encode : resolution -> Yojson.Basic.t
  (** Serialize a resolution for the plan's metadata. *)

  val packages : application -> resolution -> Package.t list
  (** Resolved versions together with the declarations that selected them. *)

  val metadata : application -> resolution -> (string * string) list

  val plan :
    application -> resolution -> (Build_plan.t, Kiln_core.Error.t) result
  (** Generate a validated plan. *)
end

type t = (module S)

type selection = { provider : t; detected : string list }
(** Selected implementation and the independently detected public names. *)

val select :
  t list ->
  requested:string option ->
  Source.t ->
  (selection, Kiln_core.Error.t) result
(** Detect every provider and prefer explicit evidence over fallback evidence.
    Restrict selection to a requested public name when supplied, allowing an
    undetected implementation if that name has no detection evidence. Require a
    unique candidate and propagate malformed metadata. [detected] always lists
    the strongest public detections independently of the request. *)
