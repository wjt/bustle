#!/bin/sh

bustle_datadir="$(dirname ${0}})"
export bustle_datadir

bustle="${bustle_datadir}"/dist/build/bustle/bustle

if [ -e $bustle ]; then
    exec $bustle "${@}"
else
    echo "Bustle hasn't been built yet; consult the README"
    exit 1
fi
