#!/bin/sh
set -e

root="$(dirname ${0}})"

bustle_datadir="${root}"/data
export bustle_datadir

bustle="${root}"/dist/build/bustle/bustle

cabal build 1>&2
exec $bustle "${@}"
