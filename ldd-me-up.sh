#!/bin/sh
set -e

ldd $1 | perl -lne 'm{(lib(?:ffi|gmp|pcap)\S+) => (\S+) } and print $2'
