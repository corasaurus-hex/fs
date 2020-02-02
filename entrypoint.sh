#!/usr/bin/env bash

if [[ $# -ne 0 ]]; then
    echo "Running: $*"
    exec bash -l -c "$*"
else
    exec bash -l -i
fi
