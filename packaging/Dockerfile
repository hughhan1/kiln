FROM alpine:3.23@sha256:85fe1e81d6758c208f3e1eed4338a1997e19d4be002d4dd32d3100c9a8c010a0 AS build
RUN apk add --no-cache build-base curl git m4 patch pkgconf linux-headers xz \
    opam=2.5.2-r0 openssl-dev openssl-libs-static ca-certificates
# Build curl without optional dependencies so libkiln.a links with any C compiler.
RUN curl --fail --location --output /tmp/curl.tar.xz https://github.com/curl/curl/releases/download/curl-8_22_0/curl-8.22.0.tar.xz \
    && echo 'f7ef3ae8a22e521f289803fe93543eb64c329b58aa73a9e224dfd915a2a5f4f7  /tmp/curl.tar.xz' | sha256sum -c - \
    && tar -xf /tmp/curl.tar.xz -C /tmp \
    && cd /tmp/curl-8.22.0 \
    && CFLAGS="-O2 -fPIC" ./configure --prefix=/opt/curl --disable-shared --enable-static --with-openssl \
       --disable-ldap --disable-ldaps --disable-rtsp --disable-dict --disable-telnet \
       --disable-tftp --disable-pop3 --disable-imap --disable-smb --disable-smtp \
       --disable-gopher --disable-mqtt --disable-ftp --disable-file \
       --without-libpsl --without-libidn2 --without-nghttp2 --without-nghttp3 \
       --without-ngtcp2 --without-brotli --without-zstd --without-zlib \
       --without-libssh2 --without-ca-path \
       --with-ca-bundle=/etc/ssl/certs/ca-certificates.crt \
    && make -j2 && make install \
    && rm -rf /tmp/curl-8.22.0 /tmp/curl.tar.xz
ENV OPAMROOT=/opt/opam OPAMYES=1 OPAMJOBS=2
RUN opam init --bare --disable-sandboxing --no-setup default \
      git+https://github.com/ocaml/opam-repository.git#74bf8da7ffea3971801da0d3540eb43f26d60fbd \
    && opam switch create kiln ocaml-base-compiler.5.5.1
ENV PKG_CONFIG_PATH=/opt/curl/lib/pkgconfig PATH=/opt/curl/bin:$PATH
WORKDIR /src
COPY kiln_ocaml.opam* ./
RUN CURLCFLAGS="$(pkg-config --cflags libcurl)" CURLLIBS="$(pkg-config --static --libs libcurl | sed 's/-pthread/-lpthread/g')" \
    opam install . --deps-only --locked --assume-depexts
COPY . .
RUN KILN_STATIC_CURL=1 scripts/package /out
FROM scratch
COPY --from=build /out/ /
