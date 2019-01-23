#!/bin/sh
set -e

readonly CORPUS=$1; shift

eval $(opam env)

find "$CORPUS"/ -iregex '.*\(pre\|post\)\(inst\|rm\)' \
    | colis-batch
