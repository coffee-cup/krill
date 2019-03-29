# krill

Toy programming language with the following features.

- Interpreted (tree walk)
- Dynamic
- Eager
- Curried
- Immutable
- Minimal

_It is being actively developed in Haskell_

## Get a Taste

Here is a taste of the language

```
add = a b -> a + b
sum = reduce add 0
sum [1, 2, 3]
# => 6
```

## Syntax

### Lambdas

There are no top level functions in krill, only lambas. A lambda is a set of
space separated parameters followed by an arrow `->`. The body of a function is
a block. A block is an expression, or multiple expressions inside some curlies
`{}`. For example, the following two lambdas are equivalent.

```
add1 = a b -> a + b

add2 = a b -> {
  a + b
}
```

The last expression in a block is returned.

### If then else

### Loops

### Lists

### I/O
