#!/bin/sh
set -e

root="$(dirname ${0}})"

# Cabal generates code that uses this
bustle_datadir="${root}"
# For Gtk+'s benefit
XDG_DATA_HOME=$bustle_datadir/data
export bustle_datadir XDG_DATA_HOME

bustle="${root}"/dist/build/bustle/bustle

cabal build 1>&2
exec $bustle "${@}"
