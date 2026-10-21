(** Small result and collection combinators shared by providers. *)

val traverse : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
(** Map left to right, stopping at the first error. *)

val iter : ('a -> ('b, 'e) result) -> 'a list -> (unit, 'e) result
(** Discard successful values and stop at the first error. *)

val maximum : compare:('a -> 'a -> int) -> 'a list -> 'a option
(** Greatest element under [compare], or [None] for an empty list. *)

val inside : string -> string -> bool
(** Test equality or a directory-boundary prefix. Callers must canonicalize
    paths first. *)
