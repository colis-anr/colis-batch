#!/bin/sh
set -e

readonly CORPUS=$1; shift

find "$CORPUS"/ -iregex '.*\(pre\|post\)\(inst\|rm\)' \
    | colis-batch
