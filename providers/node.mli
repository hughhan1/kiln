(** Node manifest detection, toolchain resolution, and Dockerfile planning.
    Implements the shared provider contract. *)

include Kiln_planning.Provider.S

val frontend :
  exclude:string list ->
  Kiln_planning.Source.t ->
  (Kiln_planning.Build_plan.operation list, Kiln_core.Error.t) result
(** Copy the output of a root package.json [build] script into another
    provider's build stage; nothing without one. The install, from the manifests
    before the source unless it needs the source, and the build run on the build
    platform. The output omits node_modules. *)
