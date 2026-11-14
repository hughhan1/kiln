(** Validated preparation inputs: the request's source and kind, and the
    commands and provider an app's railpack.json selects. *)

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

type t = private {
  source : string;
  install_steps : Commands.t;
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
          change through these commands. Unconfigured, it is the marker alone.
      *)
  provider : string option;
  build : Commands.t;  (** Configured [steps.build.commands]; none for a run. *)
  kind : kind;
}

val development : (string * Yojson.Basic.t) list -> bool
(** Whether the JSON request is a development request: an empty [development]
    object and no other kind. *)

val decode : (string * Yojson.Basic.t) list -> (t, Error.t) result
(** Decode the JSON request's [source] and its one kind: an [image] object with
    an optional [start] argument vector, or a [run] object. A request naming no
    kind plans an image. *)

val start : t -> Build_plan.argv option
(** The startup an image request names. *)

val configure : t -> Source.t -> (t, Error.t) result
(** Read the source's railpack.json, when present, and apply its [provider], its
    [steps.install] commands and, except for a run, its [steps.build] commands.
*)

val replaces_install : t -> bool
(** Whether install commands without inheritance replace the provider's
    installation, so it must see the whole source. *)
