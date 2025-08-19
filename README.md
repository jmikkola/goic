
# goic

The name comes from merging "Go" and "c" (plus another letter to make it sound better).
The idea is to build a small c-like language with more go-like syntax.

Current status: The compiler can handle some simple integer programs, like the
fibonacci sequence.

## Example code

```go
module main

func main() Void {
    var a Int = 1
    var b Int = 1

    while a < 1000 {
        printint(a)
        putchar(10)

        var temp Int = a
        a = a + b
        b = temp
    }
}

func printint(n Int) Void {
    if n / 10 > 0 {
        printint(n / 10)
    }
    putchar(n % 10 + 48)
}
```

## Building

`cabal build`

## Running

This depends on having `yasm` installed.

## Next

- Add floats (it would be fun to be able to print the mandelbot set)
- Add arrays
- Add structs
- Add string operations
- Handle function pointers
- Add a small stdlib (e.g. reading files)?
- Start some optimizations
- Support modules

A crazy stretch goal could be to make the compiler self-host.

### for building floats

milestone: usable floats

- some way to print them... (should I upgrade the print machinery first?)
    - printf isn't working, due to a few things:
    - the need to set eax to 1 indicating one vararg
    - (maybe) the use of mov rax, str0 instead of lea rax, [rel str0]
    - the fact that I'm defining my own _start instead of using the one from glibc

Temp notes on how to run printf:

```
extern	printf

section	.data
	str0 db "%f", 10, 0
    pi dq 3.14159

section	.text
global	main
main:
	push	rbp
	mov	rbp, rsp
    movsd	xmm0, qword [rel pi]
	lea	rdi, [rel str0]
    mov rax, 1
	call	printf wrt ..plt
	mov	rsp, rbp
	pop	rbp
	ret

section .note.GNU-stack noalloc noexec nowrite progbits

;;; Compilation:
;;; yasm  -f elf64 main.asm -o main.o
;;; gcc -o main main.o

;;; Important details:
;;; - setting eax to the number of varargs
;;; - [rel pi] instead of [pi] to get the value
;;; - `wrt ..plt` after the call instruction to make it relocatable
;;; - linking with gcc instead of calling ld directly
;;; - the section directive at the end to make gcc happy about the stack

;;; Details that turned out to not be important:
;;; - .rodata instead of .data
```


milestone: casting

- add syntax for casting between floats and ints
- add typechecking for casts
- add cast instructions

# Development notes

A way to try out things in the parser quickly:

- Run `cabal repl` to get a repl
- Parse something with a command like `parse assignPointer "test" "*n = *n + 1"`

Compiling and running all examples:

```bash
# compiling
for F in examples/*.gc; do echo $F; ./goic $F; done
# running
for F in examples/*; do if [[ -x $F ]]; then echo $F; ./$F > /dev/null; echo $?; fi; done
```
