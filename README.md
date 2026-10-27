# Kiln

[![OCaml](https://img.shields.io/badge/OCaml-5.5.1-EC6813?logo=ocaml)](kiln_ocaml.opam)
[![MIT License](https://img.shields.io/badge/license-MIT-green)](LICENSE)

Kiln resolves application toolchains and prepares Dockerfiles for BuildKit.

> ⚠️ **Disclaimer:** This entire repository is vibe-coded.

## Install

```sh
curl -LsSf https://github.com/hughhan1/kiln/releases/latest/download/install.sh | sh
```

## CLI

### Prepare a Dockerfile

Inspect an application and resolve its toolchain.

```sh
kiln prepare . --plan-out /tmp/Dockerfile --info-out /tmp/kiln-info.json
kiln prepare . --show-plan --hide-pretty-plan
```

<details open>
<summary><code>kiln prepare --help</code></summary>

Usage and options, abridged:

```text
Usage: kiln prepare [OPTIONS] DIRECTORY

--plan-out FILE         Write the Dockerfile
--info-out FILE         Write JSON result metadata
--show-plan             Print the Dockerfile
--hide-pretty-plan      Suppress the preparation summary
--provider NAME         Select an application provider
--build-cmd COMMAND     Override the build shell command
--start-cmd COMMAND     Override the startup shell command
--start-arg ARG         Startup argv element; repeat in order
--error-missing-start   Require a startup command
--config-file FILE      Application configuration (default: railpack.json)
--previous PKG@VERSION  Previous package version; repeat per package
--help[=FMT], -h        Show help (auto, pager, groff, or plain)
```

</details>

## Libraries

The C and OCaml APIs prepare build recipes in-process. Run the returned
Dockerfile with BuildKit.

### Services in a common source tree

Library requests may set `directory` to a service directory within `source`:

```json
{"source":"/project","directory":"app/users","platforms":["linux/amd64"]}
```

Provider detection, configuration and dependency installation use that directory.
The Dockerfile preserves the complete source tree under `/app` and runs build and
startup commands from `/app/app/users`. Omit `directory` for the existing root
behavior. Absolute paths, parent traversal and symlinks escaping `source` are rejected.
