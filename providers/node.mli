(** Node manifest detection, toolchain resolution, and Dockerfile planning.
    Implements the shared provider contract. *)

include Kiln_planning.Provider.S

val frontend :
  exclude:string list ->
  Kiln_planning.Source.t ->
  (Kiln_planning.Build_plan.operation list, Kiln_core.Error.t) result
(** Copy the output of a root package.json [build] script into another
    provider's build stage; nothing without one. The install and build run on
    the build platform when its dependencies pass the native install check
    there, and on the target otherwise. The output omits node_modules. *)
