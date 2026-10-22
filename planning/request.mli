(** Validated preparation inputs. Native argv overrides configured shell
    startup; configuration overrides request shell, install, and build commands.
*)

type startup

type t = private {
  source : string;
  environment : Environment.t;
  secrets : Environment.Names.t;
  install : Build_plan.argv option;
      (** [INSTALL_CMD], replacing the provider's install command; cleared by
          configured install commands. *)
  install_steps : Commands.t option;
      (** Configured [steps.install.commands], replacing the provider's install
          block: its dependency manifest copies and installation. Environment
          setup the runtime depends on, such as Python's [/opt/venv], stays
          outside the block. The marker ["..."] stands for that block, so
          commands before it are cached on the base image alone and commands
          after it on the dependency set. Without the marker the provider copies
          the whole source first. The full source copy and build commands follow
          either way. When Python installs from manifests, the image takes
          [/opt/venv] from a stage that ends with this block, so changes build
          commands make to the venv are not part of the image; dependencies
          change through these commands. *)
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

val install_commands :
  t -> Build_plan.operation list -> Build_plan.operation list
(** Expand configured install commands and inheritance against a provider's
    install block. *)

val replaces_install : t -> bool
(** Whether a custom install command or install commands without inheritance
    replace the provider's installation, so it must see the whole source. *)

val install_command :
  t -> string list -> (Build_plan.argv, Kiln_core.Error.t) result
(** Use a custom install command or validate the provider's default argv. *)

val version : t -> string -> string option
(** The nonblank [RAILPACK_<NAME>_VERSION] override. *)
