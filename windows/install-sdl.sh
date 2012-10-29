#!/bin/bash

set -ex

if [ -z "$1" ]; then
  SANDBOX="$PWD/../cabal-dev"
else
  SANDBOX=$1
fi

cabal_dev="cabal-dev --sandbox=$SANDBOX"

# Replace this with the path to an unpacked SDL-devel-mingw.
# Also, make sure to have unpacked the dev files for SDL_image into that same
# directory tree.
SDL_PREFIX=$HOME/Source/local/SDL-1.2.15
SDL_image_PREFIX=$HOME/Source/local/SDL_image-1.2.12

export PATH=$SDL_PREFIX/bin:$SDL_PREFIX/lib:$SDL_image_PREFIX/lib/x86:$PATH

$cabal_dev install SDL \
  --extra-include-dirs=$SDL_PREFIX/include/SDL \
  --extra-lib-dirs=$SDL_PREFIX/lib


if [ ! -d SDL-image-* ]; then
  cabal-dev unpack SDL-image

  # patch SDL-image
  pushd SDL-image-*
  cp ../patches/SDL-image/configure .
  patch -p1 < ../patches/SDL-image/Version.hsc.patch
  popd
fi

$cabal_dev install ./SDL-image-* \
  --extra-include-dirs=$SDL_PREFIX/include/SDL \
  --extra-lib-dirs=$SDL_PREFIX/lib \
  --extra-include-dirs=$SDL_image_PREFIX/include \
  --extra-lib-dirs=$SDL_image_PREFIX/lib/x86
