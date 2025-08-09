
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

milestone: parse programs with floats

- syntax for the types
- syntax for the floats themselves (just `floating` in Text.Parsec.Number?)

milestone: handle floats inside of functions

- extend type checking to understand floats
- change compilation to pay attention to whether the values are floats
    - how will this info be communicated to the compiler? one idea is to add distinct operations for float math, like PlusFloat, DivFloat, etc
- add instructions for float operations

milestone: usable floats

- update function calls, function returns, and argument loading to understand floats
- some way to print them... (should I upgrade the print machinery first?)

# Development notes

A way to try out things in the parser quickly:

- Run `cabal repl` to get a repl
- Parse something with a command like `parse assignPointer "test" "*n = *n + 1"`

Compiling and running all examples:

```bash
# compiling
for F in examples/*.gc; do echo $F; ./goic $F; done
# running
for F in examples/*; do if [[ -x $F ]]; then echo $F; ./$F > /dev/null; fi; done
```
