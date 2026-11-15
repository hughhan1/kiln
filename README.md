# Kiln

[![OCaml](https://img.shields.io/badge/OCaml-5.5.1-EC6813?logo=ocaml)](kiln_ocaml.opam)
[![MIT License](https://img.shields.io/badge/license-MIT-green)](LICENSE)

Kiln selects application toolchains from pins in source and prepares Dockerfiles
for BuildKit. Planning needs no network; `scripts/bump-pins` refreshes the pins
for each release.

> ⚠️ **Disclaimer:** This entire repository is vibe-coded.

## Use

Kiln is a library. Link the C SDK that `scripts/package PREFIX` builds
(`ffi/kiln.h`, `libkiln.a`, `kiln.pc`) and pass `kiln_plan` a JSON request:

```json
{"source": "/abs/app", "image": {"start": ["node", "server.js"]}}
```

A request names its `source` and one kind. An `image` request, the default,
returns `{"dockerfile": "..."}` to build with BuildKit. `"development": {}`
returns what a development environment runs: `{"inputs": [...], "install":
[...], "build": [...]}`. A `run` request returns the Dockerfile of an image that
runs a command in the app's image. A failure returns `{"error": "..."}`. Kiln
reads an app's manifests and nothing else: a `railpack.json` is ignored.
