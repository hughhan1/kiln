(** A validated Dockerfile dependency graph. Abstract stages can reference only
    stages already constructed, making cycles unrepresentable through this
    interface. *)

type argv = private { executable : string; arguments : string list }

val argv : string list -> (argv, Kiln_core.Error.t) result
(** Require a nonempty executable and NUL-free arguments. Empty argument strings
    are preserved. *)

val arguments : argv -> string list
(** Executable followed by its arguments, in invocation order. *)

val shell : string -> (argv, Kiln_core.Error.t) result
(** Run a nonblank script with [/bin/sh -c]; reject embedded NUL bytes. *)

type environment

val environment :
  (string * string) list -> (environment, Kiln_core.Error.t) result
(** Require at least one variable, nonempty alphanumeric/underscore names, and
    values without NUL, newline, or carriage return. *)

type image

val image : string -> (image, Kiln_core.Error.t) result
(** Accept a repository pinned to a lowercase, 64-digit SHA-256 digest. *)

val image_ref : image -> string
(** The validated repository and digest in Docker reference syntax. *)

type stage
type image_reference
type base = Image of image | Stage of stage
type input = Context | Output of stage | External of image_reference

type operation =
  | Run of argv
  | Copy of {
      input : input;
      paths : string list;
      exclude : string list;
      destination : string;
      owner : (int * int) option;
    }
  | Workdir of string
  | Env of environment
  | User of int * int
  | Entrypoint of argv option
  | Start of argv option
  | Prepend_path of string
  | Write of { path : string; contents : string; mode : int }

val external_image : string -> (input, Kiln_core.Error.t) result
val validate_operations : operation list -> (unit, Kiln_core.Error.t) result

val stage : base -> operation list -> (stage, Kiln_core.Error.t) result
(** Validate paths, exclusions, and nonnegative user IDs before admitting
    operations to the graph. References name already constructed stages. *)

type t = stage

val stages : t -> stage list
(** Dependencies before dependents; shared stage objects occur once. *)

val base : stage -> base
(** The image or preceding stage inherited by this stage. *)

val operations : stage -> operation list
(** Operations in Dockerfile execution order. *)

val startup : t -> argv option
(** The last CMD in this stage, or an inherited CMD unless ENTRYPOINT resets it.
*)

val dockerfile :
  ?directory:string -> ?secrets:Environment.Names.t -> t -> string
(** Render dependencies once, with stable stage names and JSON-escaped argument
    arrays. Mount each supplied secret as a required environment variable on RUN
    operations; values are never written into the Dockerfile. *)

val run : string list -> (operation, Kiln_core.Error.t) result
(** Construct a RUN operation after validating its argv. *)

val context : ?exclude:string list -> string list -> string -> operation
(** Copy source snapshot paths; path and exclusion validation happens in
    [stage]. *)

val copy :
  ?exclude:string list ->
  ?owner:int * int ->
  stage ->
  string list ->
  string ->
  operation
(** Copy paths from an existing stage; [stage] validates paths, exclusions, and
    ownership. *)
