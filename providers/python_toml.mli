(** TOML boundary helpers that distinguish absent keys from malformed present
    values. *)

val read :
  Kiln_planning.Source.t -> string -> (Otoml.t, Kiln_core.Error.t) result
(** Read and parse a contained file, labelling syntax errors with its path. *)

val field :
  Otoml.t ->
  string list ->
  (Otoml.t -> 'a) ->
  ('a option, Kiln_core.Error.t) result
(** Decode an optional key path; convert TOML type errors to metadata errors. *)

val strings : Otoml.t -> string list -> (string list, Kiln_core.Error.t) result
(** Decode an array of strings, defaulting to empty when absent. *)

val string : Otoml.t -> string list -> (string option, Kiln_core.Error.t) result
(** Require a string when the key is present. *)

val array :
  Otoml.t ->
  string list ->
  (Otoml.t -> 'a) ->
  ('a list, Kiln_core.Error.t) result
(** Decode array elements, defaulting to empty when absent. *)

val table :
  Otoml.t ->
  string list ->
  (Otoml.t -> 'a) ->
  ((string * 'a) list, Kiln_core.Error.t) result
(** Decode object values, preserving keys and defaulting to empty when absent.
*)

val read_optional :
  Kiln_planning.Source.t -> string -> (Otoml.t, Kiln_core.Error.t) result
(** Read TOML if the path exists in the snapshot; otherwise return an empty
    table. *)
