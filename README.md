
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

milestone: handle floats inside of functions

- change compilation to pay attention to whether the values are floats
    - how will this info be communicated to the compiler?
    - All of `compileBinaryOp` will need to change for float:
        - It needs a different instruction for the actual math or comparison
          operation
        - It also needs to use different registers
    - Maybe I can tag BinaryOp with the discovered type
- add instructions for float operations
- Update how variables are loaded and saved
- Update how the stack is pushed and popped

milestone: usable floats

- update function calls, function returns, and argument loading to understand floats
- some way to print them... (should I upgrade the print machinery first?)

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
for F in examples/*; do if [[ -x $F ]]; then echo $F; ./$F > /dev/null; fi; done
```
