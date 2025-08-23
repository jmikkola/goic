
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

- Add line numbers to error messages
- Add arrays
- Add structs
- Add a character type
- Add string operations
- Handle function pointers
- Add a small stdlib (e.g. reading files)?

Maybe:
- Start some optimizations
- Support modules

A crazy stretch goal could be to make the compiler self-host.

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
