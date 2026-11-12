(** Small result and collection combinators shared by providers. *)

val traverse : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
(** Map left to right, stopping at the first error. *)

val iter : ('a -> ('b, 'e) result) -> 'a list -> (unit, 'e) result
(** Discard successful values and stop at the first error. *)
