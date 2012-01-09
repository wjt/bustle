#!/bin/sh
set -e

root="$(dirname ${0}})"

bustle_datadir="${root}"
export bustle_datadir

bustle="${root}"/bin/bustle

exec $bustle "${@}"
