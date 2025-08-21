#!/usr/bin/env bash
set -e
yasm -Worphan-labels -g dwarf2 -f elf64 "$1.asm" -o "$1.o"
gcc -g -o "$1" "$1.o"
