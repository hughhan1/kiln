(** Provider extension contract. Application and resolution types stay private
    to each provider. *)

type detection = No_match | Fallback | Explicit

module type S = sig
  type application
  type resolution

  val id : string
  (** Stable implementation identifier used for resolution reuse. *)

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

  val startup : application -> Build_plan.argv option
  (** Selected application startup, independent of toolchain discovery. [plan]
      must emit this value as the final CMD. *)

  val fingerprint : application -> string
  (** Deterministic toolchain requirements key; source-only and startup edits
      should not change it. *)

  val resolve :
    today:string ->
    fetch:Registry.fetch ->
    application ->
    (resolution, Kiln_core.Error.t) result
  (** Discover immutable toolchain pins for the application and requested
      targets. *)

  val decode : Yojson.Basic.t -> (resolution, Kiln_core.Error.t) result
  (** Parse the provider-specific reusable resolution payload. *)

  val encode : resolution -> Yojson.Basic.t
  (** Serialize a resolution for a later [decode]. *)

  val packages : application -> resolution -> Package.t list
  (** Resolved versions together with the declarations that selected them. *)

  val metadata : application -> (string * string) list

  val legacy :
    application ->
    Yojson.Basic.t ->
    (resolution option, Kiln_core.Error.t) result
  (** Decode an unversioned resolution, returning [None] if its requirements no
      longer match. Reject unsupported formats. *)

  val plan :
    application -> resolution -> (Build_plan.t, Kiln_core.Error.t) result
  (** Generate a validated plan, rejecting resolutions incompatible with the
      application. *)
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
