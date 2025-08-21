#!/usr/bin/env bash

set -e

# Compile all
for F in examples/*.gc; do
    ./goic "$F"
done

# Execute all
for F in examples/*; do
    # find executable files
    if [[ -x "$F" ]]; then
        echo "$F"
        if ! "./$F" > /dev/null; then
            echo "$F failed"
        fi
    fi
done
