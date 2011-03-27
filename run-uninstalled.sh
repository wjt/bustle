#!/bin/sh
set -e

bustle_datadir="$(dirname ${0}})"
export bustle_datadir

bustle="${bustle_datadir}"/dist/build/bustle/bustle

cabal build
exec $bustle "${@}"
