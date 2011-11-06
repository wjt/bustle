#!/bin/sh
set -e

root="$(dirname ${0}})"

bustle_datadir="${root}"/share/bustle-0.3.0
export bustle_datadir

bustle="${root}"/bin/bustle

exec $bustle "${@}"
