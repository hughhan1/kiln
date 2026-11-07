(** Toolchain versions and image digests Kiln selects from, oldest first in each
    table. [scripts/bump-pins] regenerates the implementation with each release.
*)

type image = { version : string; image : string }
type toolchain = { release : string; builder : string; runtime : string }
type node = { node : toolchain; npm : string; lts : string option }
type tool = { tool : string; engines : string }

val node : node list
(** Every Node release with images in a major inside its support window, with
    its bundled npm. *)

val npm : tool list
(** npm releases, with the Node range each supports. *)

val pnpm : tool list
(** pnpm releases, with the Node range each supports. *)

val python : toolchain list
(** Every stable Python release on Debian trixie. *)

val uv : image list
val bun : image list
val deno : image list

val golang : image list
(** Every Go release from 1.21, and the newest patch of each older minor. *)

val debian : string
(** Debian bookworm-slim, the runtime for Go binaries. *)
