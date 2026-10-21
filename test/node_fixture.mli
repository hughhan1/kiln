(** Deterministic Node release, lifecycle, and registry responses. *)

val image_index : string -> string
(** Image-index bytes with a version annotation, giving each tag a distinct
    digest. *)

val image_urls : (string * string) list
(** Manifest endpoints and their fixture bodies. *)

val discovery : (string * string) list -> Kiln_io.Driver.discovery
(** Override selected response bodies; reject unexpected URLs and authorization.
*)

val fetch : Kiln_io.Driver.discovery
(** Unmodified fixture discovery for the standard provider contract cases. *)
