(** Small result and collection combinators shared by providers. *)

val traverse : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
(** Map left to right, stopping at the first error. *)

val iter : ('a -> ('b, 'e) result) -> 'a list -> (unit, 'e) result
(** Discard successful values and stop at the first error. *)

val select :
  parse:('row -> ('version, 'e) result) ->
  compare:('version -> 'version -> int) ->
  accept:('version -> 'row -> bool) ->
  none:'e ->
  'row list ->
  ('version * 'row, 'e) result
(** The row with the greatest parsed version among those accepted, or [none]. *)

val inside : string -> string -> bool
(** Test equality or a directory-boundary prefix. Callers must canonicalize
    paths first. *)
