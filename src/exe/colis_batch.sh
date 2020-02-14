#!/bin/sh
set -euC

abs()(cd "$1";pwd)

if [ $# -le 1 ]; then
    printf 'Argument required.\n'
    exit 1
fi

cmd=$1
shift

export PATH=$():$PATH
"$(basename "$0")"-"$cmd" "$@"
