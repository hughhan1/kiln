(** Shared package.json interpretation for JavaScript providers. Integrity
    values are constructible only through their validating parser. *)

type algorithm = [ `SHA224 | `SHA256 | `SHA384 | `SHA512 ]
type integrity = private Integrity of algorithm * string
type declaration = { name : string; requested : string option; source : string }

type package = {
  engines : (string * string) list;
  runtimes : declaration list;
  managers : declaration list;
  declared_integrity : integrity option;
  scripts : (string * string) list;
  main : string option;
  references : string list;
  workspaces : string list;
  needs_source : bool;
      (** Installation needs the whole source: injected dependencies or pnpm
          settings. Local [references] are judged by the provider. *)
}

val algorithm : algorithm -> string
(** Lowercase digest name used by packageManager integrity declarations. *)

val integrity_text : integrity -> string
(** Canonical algorithm.hex encoding of a validated digest. *)

val integrity : string -> (integrity, Kiln_core.Error.t) result
(** Parse a supported digest algorithm and a correctly sized lowercase hex
    digest. *)

val package : string -> (package, Kiln_core.Error.t) result
(** Parse package.json, including engine declarations, scripts, workspaces, and
    local references. *)

val has_script : package -> string -> bool
(** Whether a named script exists and is nonempty. *)

val startup :
  package ->
  override:Kiln_planning.Build_plan.argv option ->
  script:string list ->
  main:string list ->
  (Kiln_planning.Build_plan.argv option, Kiln_core.Error.t) result
(** Prefer an override, then the start script, then a nonempty main entry.
    Return [None] when no startup is declared. *)

val load :
  Kiln_planning.Source.t -> ((string * package) list, Kiln_core.Error.t) result
(** Read every package.json in the snapshot and check all local references for
    containment. *)

val root : Kiln_planning.Source.t -> (package, Kiln_core.Error.t) result
(** Read only the root package.json for provider detection. *)

val requests :
  path:string -> declaration list -> Kiln_planning.Package.requirement list

val requirements :
  path:string ->
  runtime:string ->
  package ->
  (Kiln_planning.Package.requirement list, Kiln_core.Error.t) result
(** Collect declarations for a runtime, rejecting declarations naming a
    different runtime or manager. *)
