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
{"source": "/abs/app", "start": ["node", "server.js"]}
```

It returns `{"dockerfile": "..."}` to build with BuildKit, or, for a request with
`"development": true`, what a development environment runs: `inputs`,
`install`, `build` and `start`. A failure returns `{"error": "..."}`. An app's
`railpack.json` may select its `provider` and its `steps.install` and
`steps.build` commands.
