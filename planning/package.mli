(** The selection of a pinned version that satisfies version declarations. *)

val select :
  tool:string ->
  declarations:string list ->
  version:('row -> string) ->
  parse:(string -> ('version, Error.t) result) ->
  accept:('version -> 'row -> bool) ->
  'row list ->
  ('version * 'row, Error.t) result
(** The newest accepted row of a table ordered oldest first. Otherwise fail
    naming the declarations and every version the table supports. *)
