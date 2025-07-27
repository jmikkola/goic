#!/usr/bin/env bash
set -e
yasm -Worphan-labels -g dwarf2 -f elf64 "$1.asm" -o "$1.o"
ld -g -o "$1" "$1.o" -lc --dynamic-linker /lib64/ld-linux-x86-64.so.2 -e _start
