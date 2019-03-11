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
add10 = add 10
x = add10 10
# x = 20
```

## TODO

- [x] Repl
- [x] Parser
- [x] Evaluator
- [x] Closures
- [x] Recursive closures
- [x] Lists
- [ ] Built in functions (print, length, etc.)
- [ ] Standard library
- [ ] Error handling

## Syntax

### Lambdas

There are no top level function in krill, only lambas. A lambda is a set of
space separated parameters followed by an arrow `->`. The body of a function is
a block. A block is an expression, or multiple expressions instead some curlies
`{}`. For example, the following two lambdas are equivalent.

```
add1 = a b -> a + b

add2 = a b -> {
  a + b
}
```

The last expression in a block is returned.

### If then else

You can define an if statement with the `if condition then trueBlock else
elseBlock` syntax. For example,

```
if x == 2 then "its two!" else {
  "its not two."
}
```

### Lists

You define lists the same way as in any programming language.

```
[1, 2, 3]
```

