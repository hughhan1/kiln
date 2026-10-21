(** Validated preparation inputs. Native argv overrides configured shell
    startup; configuration overrides request shell and build commands. *)

type startup

type t = private {
  source : string;
  environment : Environment.t;
  secrets : Environment.Names.t;
  install : Build_plan.argv option;
  platforms : Platforms.t;
  start : startup option;
  error_missing_start : bool;
  provider : string option;
  build : Commands.t option;
  config_file : string;
  variables : Build_plan.environment option;
  previous : Yojson.Basic.t option;
  previous_versions : (string * string) list;
}

val decode : (string * Yojson.Basic.t) list -> (t, Kiln_core.Error.t) result
(** Decode JSON request fields and defaults. Reject unsupported/empty target
    sets and simultaneous argv and shell startup. *)

val start : t -> Build_plan.argv option
(** Extract the selected startup argv, regardless of how it was specified. *)

val build_command : t -> Build_plan.argv option -> Build_plan.operation list
(** Expand configured operations and inheritance against a provider default. *)

val configure : t -> Source.t -> (t, Kiln_core.Error.t) result
(** Read contained JSONC configuration and apply its supported fields; an absent
    default railpack.json is allowed. *)

val defaults :
  t -> string -> Package.requirement list -> Package.requirement list
(** Environment version overrides replace declarations; previous versions are
    used only when neither was supplied. *)

val install_command :
  t -> string list -> (Build_plan.argv, Kiln_core.Error.t) result
(** Use a custom install command or validate the provider's default argv. *)

val version : t -> string -> string option
(** The nonblank [RAILPACK_<NAME>_VERSION] override. *)
