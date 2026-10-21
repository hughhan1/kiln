(** Filesystem helpers; filesystem failures are raised for the driver to
    translate. *)

val absolute : string -> string
(** Resolve existing paths and normalize missing path components, including [.]
    and [..]. Relative paths are resolved from the working directory. *)

val mkdir : perm:int -> string -> unit
(** Create missing parent directories with the requested permissions. *)
