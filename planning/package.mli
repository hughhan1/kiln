(** Version declarations and the selection of a pinned version that satisfies
    them. *)

type requirement = { version : string; source : string }

val optional : string -> string option -> requirement list
(** Preserve a declaration's text and origin when it is present. *)

val versions : requirement list -> string list

val select :
  tool:string ->
  declarations:requirement list ->
  version:('row -> string) ->
  parse:(string -> ('version, Error.t) result) ->
  accept:('version -> 'row -> bool) ->
  'row list ->
  ('version * 'row, Error.t) result
(** The newest accepted row of a table ordered oldest first. Otherwise fail
    naming the declarations and every version the table supports. *)
