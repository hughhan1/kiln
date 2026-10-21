(** Contract-test assertions, isolated snapshots, and deterministic discovery.
*)

val check : string -> bool -> unit
(** Fail with the supplied label when an observable contract is false. *)

val ok : ('a, Kiln_core.Error.t) result -> 'a
(** Unwrap success or fail with the domain diagnostic. *)

val contains : string -> string -> bool
(** Substring assertion predicate, with haystack first. *)

val write : string -> string -> string -> unit
(** Write bytes to a path relative to a snapshot root. *)

val request : string -> (string * Yojson.Basic.t) list -> string
(** JSON request with a source root and both platforms; extra fields override
    defaults. *)

val test : string -> (unit -> unit) -> unit
(** Run a named case and retain failures for [finish], allowing later cases to
    run. *)

val temporary : (string -> 'a) -> 'a
(** Supply a temporary directory and remove it on success or exception. *)

val snapshot : (string * string) list -> string -> (string -> unit) -> unit
(** Run a named case against temporary source files. *)

val finish : unit -> unit
(** Exit unsuccessfully if any named case failed. *)

val read : string -> string
(** Read the exact file bytes for artifact assertions. *)

val environment : (string * string) list -> string array
(** Overlay variables on the current process environment. *)

val run :
  ?env:string array ->
  ?exit_code:int ->
  ?errors:Buffer.t ->
  string ->
  string list ->
  string
(** Capture stdout and require the expected exit status (default zero),
    including stderr in failure diagnostics and the optional [errors] buffer.
    Reap or terminate the child before returning. *)

val registry_index : string
(** Fixture image index advertising both supported Linux architectures. *)

val registry :
  (string * string list) list ->
  string ->
  string option ->
  (string, Kiln_core.Error.t) result
(** Serve only declared tag catalogs and manifests; reject unexpected URLs or
    bearer tokens. *)
