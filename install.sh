#!/bin/sh
set -eu

fail() { echo "kiln: $*" >&2; exit 1; }
[ "$#" -eq 0 ] || fail 'usage: install.sh'
command -v curl >/dev/null 2>&1 || fail 'curl is required.'
case "$(uname -s)" in
  Darwin)
    os=darwin
    [ "$(sw_vers -productVersion | cut -d . -f 1)" -ge 15 ] || fail 'macOS 15 or later is required.'
    ;;
  Linux) os=linux ;;
  *) fail 'Supported systems: macOS and Linux.' ;;
esac
case "$(uname -m)" in
  x86_64|amd64) arch=amd64 ;;
  arm64|aarch64) arch=arm64 ;;
  *) fail 'Supported architectures: AMD64 and ARM64.' ;;
esac
if command -v sha256sum >/dev/null 2>&1; then
  checksum() { sha256sum "$1"; }
elif command -v shasum >/dev/null 2>&1; then
  checksum() { shasum -a 256 "$1"; }
else
  fail 'Install sha256sum or shasum to verify the download.'
fi

repo=${KILN_REPOSITORY:-hughhan1/kiln}
release=$(curl -fsSL -o /dev/null -w '%{url_effective}' "https://github.com/$repo/releases/latest")
version=${release##*/}
[ -n "$version" ] && [ "$version" != latest ] || fail 'Cannot find the latest release.'
archive="kiln-${os}_${arch}.tar.gz"
prefix=${KILN_PREFIX:-"$HOME/.local"}
stage=$(mktemp -d)
trap 'rm -rf "$stage"' EXIT
trap 'exit 1' HUP INT TERM
for asset in "$archive" checksums.txt; do
  curl -fsSL "https://github.com/$repo/releases/download/$version/$asset" -o "$stage/$asset"
done
expected=$(awk -v name="$archive" '$2 == name {print $1}' "$stage/checksums.txt")
actual=$(checksum "$stage/$archive" | cut -d ' ' -f 1)
[ -n "$expected" ] && [ "$actual" = "$expected" ] || fail 'Archive checksum mismatch; nothing installed.'
tar -xzf "$stage/$archive" -C "$stage" bin/kiln include/kiln.h lib/libkiln.a lib/pkgconfig/kiln.pc
for file in bin/kiln include/kiln.h lib/libkiln.a lib/pkgconfig/kiln.pc; do
  [ -f "$stage/$file" ] && [ ! -L "$stage/$file" ] || fail "Missing package file: $file"
done
mkdir -p "$prefix/bin" "$prefix/include" "$prefix/lib/pkgconfig"
install -m 755 "$stage/bin/kiln" "$prefix/bin/kiln"
install -m 644 "$stage/include/kiln.h" "$prefix/include/kiln.h"
install -m 644 "$stage/lib/libkiln.a" "$prefix/lib/libkiln.a"
install -m 644 "$stage/lib/pkgconfig/kiln.pc" "$prefix/lib/pkgconfig/kiln.pc"
echo "Installed Kiln $version and its native SDK in $prefix"
case ":$PATH:" in
  *":$prefix/bin:"*) ;;
  *) printf 'Add %s/bin to your PATH.\n' "$prefix" ;;
esac
