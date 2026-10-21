(** Docker Hub discovery with caller-supplied HTTP access. *)

type fetch = string -> string option -> (string, Kiln_core.Error.t) result

val memoize : fetch -> fetch
(** Cache successful responses by URL and bearer token for one preparation. *)

val tags : fetch -> string -> (string list, Kiln_core.Error.t) result
(** Fetch the repository's Docker Hub tag catalog. *)

val image :
  fetch ->
  platforms:Platforms.t ->
  repository:string ->
  string ->
  (Build_plan.image, Kiln_core.Error.t) result
(** Require every selected platform in the image index and pin the digest of
    that index. *)

val sha256 : string -> string
(** Lowercase SHA-256 digest of the exact bytes supplied. *)

val select :
  fetch ->
  repository:string ->
  exact:'a option ->
  parse:(string -> 'a option) ->
  compare:('a -> 'a -> int) ->
  matches:('a -> bool) ->
  unavailable:Kiln_core.Error.t ->
  ('a, Kiln_core.Error.t) result
(** Exact versions need no catalog lookup. Parsing and ordering belong to the
    caller's version domain. *)
