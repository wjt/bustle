#!/bin/sh

bustle_datadir="$(dirname ${0}})"
export bustle_datadir

exec "${bustle_datadir}"/dist/build/bustle/bustle "${@}"
