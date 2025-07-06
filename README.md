
# goic

The name comes from merging "Go" and "c" (plus another letter to make it sound better).
The idea is to build a small c-like language with more go-like syntax.

Current status: There's a parser, a typechecker, and I'm just starting on the compiler.

The eventual goal is to try to learn x86_64 assembly.

## Next

- Fix linking
- Variables
- Other math instructions
- Function calls
- Control flow

## Assembling and Linking

Assembler command:

    yasm -Worphan-labels -g dwarf2 -f elf64 main.asm

Link command:

    ld -g -o main main.o -lc --dynamic-linker /lib64/ld-linux-x86-64.so.2 -e _start

It seems like it needs

```
    ;; exit
    mov rax, SYS_exit
    mov rdi, EXIT_SUCCESS
    syscall
```

at the end to not segfault when main exits.
