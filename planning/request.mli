(** Validated preparation inputs: the request's source and startup, and the
    commands and provider an app's configuration file selects. *)

type t = private {
  source : string;
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
  start : Build_plan.argv option;
  provider : string option;
  build : Commands.t option;
  config_file : string;
}

val decode : (string * Yojson.Basic.t) list -> (t, Error.t) result
(** Decode the JSON request's [source], [start] and [configFilePath]. *)

val build_command : t -> Build_plan.operation list -> Build_plan.operation list
(** Expand configured operations and inheritance against a provider default. *)

val configure : t -> Source.t -> (t, Error.t) result
(** Read contained JSONC configuration and apply its [provider] and
    [steps.install] and [steps.build] commands; an absent default railpack.json
    is allowed. *)

val install_commands :
  t -> Build_plan.operation list -> Build_plan.operation list
(** Expand configured install commands and inheritance against a provider's
    install block. *)

val replaces_install : t -> bool
(** Whether install commands without inheritance replace the provider's
    installation, so it must see the whole source. *)
