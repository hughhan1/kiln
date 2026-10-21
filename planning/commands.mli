type t

val decode : assets:(string * string) list -> t Json.decoder
(** Parse ordered RUN, PATH, COPY, and FILE commands, resolving FILE assets.
    Command objects must describe exactly one operation. Strings run through the
    shell; [cmd] objects are parsed as executable-and-arguments. *)

val shell : string -> (t, Kiln_core.Error.t) result
(** A single shell command, without configuration shorthand or inheritance. *)

val expand : t -> Build_plan.operation list -> Build_plan.operation list
(** Replace each inheritance marker with the provider's commands, in order. *)

val paths : t -> string list
(** PATH additions in command order, for propagation to the runtime. *)
