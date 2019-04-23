# krill

Toy programming language with the following features.

- Dynamic
- Eager
- Curried
- Immutable
- Minimal

## Get a Taste

All the even factorials between 0 and 10

```
even = x -> x % 2 == 0
fact = n -> if n == 0 then 1 else n * (fact $ n - 1)
filter even $ map fact [0..10]
# => [2,6,24,120,720,5040,40320,362880,3628800]
```

Sum numbers between 1 and 1000

```
square = x -> x * x
sumOddSquares = sum . filter (not . even) . map square
sumOddSquares [1..100]
# => 166650
```

## Syntax

### Literals

- Numbers `1` and `1.5`
- Strings `"hello"`
- Chars `'a'`
- Booleans `true` and `false`
- Atoms `:hello`
- Unit `()`

### Variables

You can define variables easily

``` text
x = 2
```

Variables are immutable

``` text
x = 2
x = 1 # Error: Variable `x` Already Bound
```

### Lambdas

There are no top level functions in krill, only lambas. A lambda is a set of
space separated parameters followed by an arrow `->`. The body of a function is
a block. A block is an expression, or multiple expressions inside some curlies
`{}`. For example, the following two lambdas are equivalent.

```
add1 = a b -> a + b

add2 = a b -> {
  result = a + b
  result
}
```

The last expression in a block is returned.

### Lists

Lists are as you expect

``` text
l = [1,2,3]
l[1]
# => 2
```

You can also creates lists with ranges that work the same way as in Haskell.

``` text
[1..10]
# => [1,2,3,4,5,6,7,8,9,10]

[1,3..10]
# => [1,3,5,7,9]
```

### If expressions

Since everything in krill is an expression, if's must return a value, meaning
the else block must be provided.

``` text
if x == 2 then "x is two!" else {
  "x is not two."
}
```

### For Loops

``` text
for x in [1..5] { print x }
# => 1
# => 2
# => 3
# => 4
# => 5
```

### I/O

There are three I/O functions. One for reading, writing, and appending.


``` text
writeFile "/var/tmp/foo" "hello world"

readFile "/var/tmp/foo"
# => "hello world"

appendFile "/var/tmp/foo" "\nhow are you?"

readFile "/var/tmp/foo"
# => hello world
# => how are you?
```

# Installation

Build it yourself with [Haskell](https://www.haskell.org/).

1.  Install [Stack](https://docs.haskellstack.org/en/stable/README/), which we will use to compile the Haskell code.
2.  Clone the repository and `cd` into the directory.
3.  Run `stack setup` to download the correct version of GHC.
4.  Run `stack install --local-bin-path=out/build` to build the application.
5.  Copy the application binary at `out/build/krill` to wherever you need it to go.

## Repl

Running `krill` with no arguments will launch a repl. You can `.help` in the
repl for a list of available commands. Commands are all prefixed with the `.`.

## Run a file

You can run a file with `krill path/to/file.kr`.
