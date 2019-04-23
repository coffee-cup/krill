# krill

Toy programming language with the following features.

- Dynamic
- Eager
- Curried
- Immutable
- Minimal

For more information and docs head to [krill.jakerunzer.com](https://krill.jakerunzer.com)

## Get a Taste

All the even factorials between 0 and 10

```haskell
even = x -> x % 2 == 0
fact = n -> if n == 0 then 1 else n * (fact $ n - 1)
filter even $ map fact [0..10]
# => [2,6,24,120,720,5040,40320,362880,3628800]
```

Sum numbers between 1 and 1000

```haskell
square = x -> x * x
sumOddSquares = sum . filter (not . even) . map square
sumOddSquares [1..100]
# => 166650
```
