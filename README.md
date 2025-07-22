
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

- Fix stack alignment
- Other math instructions
- Support more than 6 arguments
- Add floats (it would be fun to be able to print the mandelbot set)
- Add arrays
- Add strings
- Add structs
- Add a small stdlib (e.g. reading files)?
- Start some optimizations
- Support modules

A crazy stretch goal could be to make the compiler self-host.

## Current

Right now the fib example (and the simpler putc example) is segfaulting. Based on chatGPT, it sounds
like this is a stack alignment issue. I'm aligning it to 8 bytes when it needs to be aligned to 16.

The issue with segfaulting seems to have gone away for no reason I can
discern. I haven't fixed the stack alignment yet.
