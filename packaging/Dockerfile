FROM alpine:3.23@sha256:85fe1e81d6758c208f3e1eed4338a1997e19d4be002d4dd32d3100c9a8c010a0 AS build
RUN apk add --no-cache build-base curl git m4 patch pkgconf linux-headers xz \
    opam=2.5.2-r0 ca-certificates
ENV OPAMROOT=/opt/opam OPAMYES=1 OPAMJOBS=2
RUN opam init --bare --disable-sandboxing --no-setup default \
      git+https://github.com/ocaml/opam-repository.git#74bf8da7ffea3971801da0d3540eb43f26d60fbd \
    && opam switch create kiln ocaml-base-compiler.5.5.1
WORKDIR /src
COPY kiln_ocaml.opam* ./
RUN opam install . --deps-only --locked --assume-depexts
COPY . .
RUN KILN_STATIC=1 scripts/package /out
FROM scratch
COPY --from=build /out/ /
