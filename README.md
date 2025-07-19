
# goic

The name comes from merging "Go" and "c" (plus another letter to make it sound better).
The idea is to build a small c-like language with more go-like syntax.

Current status: There's a parser, a typechecker, and I'm just starting on the compiler.

The eventual goal is to try to learn x86_64 assembly.

## Next

- Get enough working for a fib() function
- Variables
- Add loops
- Add char-by-char output
- Other math instructions
- Function calls
- Add a small stdlib?

## Current

Right now the fib example (and the simpler putc example) is segfaulting. Based on chatGPT, it sounds
like this is a stack alignment issue. I'm aligning it to 8 bytes when it needs to be aligned to 16.

Another reason that the fib example doesn't work is it uses rdi to pass the argument, but the caller doesn't preserve the register. I think this means that I should make all functions preserve their arguments to the stack and read them back from the stack.
