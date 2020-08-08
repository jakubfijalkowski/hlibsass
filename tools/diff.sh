#!/usr/bin/env zsh
set -euo pipefail
setopt extended_glob

pushd "${0:A:h}/.." >/dev/null

diff -y \
    <(cat Bindings/Libsass/**.hs | grep -E 'foreign import .+ (libsass|sass)_' | sed -E 's/^.+ (libsass|sass)_(.+)$/\1_\2/gm' | tr -d '"' | sort) \
    <(cat libsass/include/sass.h libsass/include/sass/*.h | grep -E 'ADDAPI .+ ADDCALL (sass|libsass).+ ?\(' | sed -E 's/^.+ (libsass|sass)_(.+?) ?\(.+$/\1_\2/gm' | awk '{$1=$1};1' | sort)

popd >/dev/null
