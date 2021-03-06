# Haskell Notes

## Stack

Use `stack` to manage ghc and installed packages. Use `package.yaml` or `Project.cabal` but not both.

View the dependency tree:

```bash
$ stack dot --external | dot -Tpng | feh -
```

## Imports

Often importing a standard package needs two imports, one to bring in the useful name of the data structure,
and a second `qualified` import to bring in useful functions that operate on the data structure:

```haskell
import Data.Map (Map)
import qualified Data.Map as Map
```

## Packages

`words` splits a string by spaces. Use `splitOn` from `Data.List.Split` to split a string by other characters.

`Data.Sequence` provides a useful random-access data structure that is faster than a List except for things that
are better suited to a List (e.g. a Stack).

## GHCI

Use `:t x` to display type information about `x`.

Use `:i x` to display type and origin information about `x`.

Use `:set m` to enable multi-line editing mode.

Use `:set -XExtensionName` to enable an extension in GHCI.

To read a file into a List of values, for example `Int`s:

```haskell
λ> content <- readFile "filename.txt"
λ> map (read :: String -> Int) content
```

## Extensions

Enabled with, for example:

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
```

Or in GHCI with `:set -XDuplicateRecordFields`.

### DuplicateRecordFields

Allows multiple datatypes to be declared in the same module with the same field names.
Without this enabled, the constructor names will clash.

### LambdaCase

Allows the use of lambda-case syntax, such as:

```haskell
\case
  p1 -> e1
  ...
  pN -> eN
```

### TupleSections

`TupleSections` is syntactic sugar that allows partial tuples to be constructed:

```haskell
f = (1, 2, ,4)
f 3  -- returns (1, 2, 3, 4)
```

It is equivalent to `\x -> (1, 2, x, 4)`.


## Language

For historical reasons, `length`, `take`, `drop`, `splitAt`, `!!` and `replicate` take or return `Int` values,
which makes them non-generic. In `Data.List` there is `genericLength`, `genericTake`, `genericDrop`, `genericSplitAt`,
`genericIndex` and `genericReplicate` which can be used for numerical purposes, such as `/`.

### Lists

`cons` (adding a new item to head with `:`) is cheaper than appending (adding a new item to the tail with `++`).
Sometimes this means it's faster to construct lists in reverse order, then reverse them to get the final result.

### Fold & Scan

`foldl` takes accumulator initial value and a list, `foldr` takes a list and an initial value.

The `++` operator is more expensive than the `:` operator, so it's better to use `foldr` when building
new lists from an existing list, e.g.:

```haskell
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
```

vs. using a left fold:

```haskell
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs,
```

Right folds work on infinite lists, but left folds do not.

*Important* - Folds can be used to implement any function where you traverse a list once,
element by element, and then return something based on that.
Whenever you want to traverse a list to return something, chances are you want a fold.

`foldl1` and `foldr1` use the first item in the list as the accumulator initial value. The downside
is that these functions throw an error on an empty list.

`scanl` and `scanr` are like `foldl` and `foldr` except that instead of producing a final accumulator value,
they add the intermediate accumulator values to a list and return that. The final accumulator value will be
at the list head (for `scanr`) or the end of the list (for `scanl`).

Use strict versions of iterators (like folds) if the stack overflows with lazy thunks.

### Functions

You can use `$` to map a value over a list of functions:

```haskell
λ> map ($ 3) [(4+), (10*), (^2), sqrt]
[7.0,30.0,9.0,1.7320508075688772]
```

### Arithmetic

To combine a number of equal-length lists together, use `transpose` and `map`:

```haskell
ghci> map sum $ Data.List.transpose [[0,1,2,3], [8,8,8,8], [0,4,9,3]]
[8,13,19,14]
```

### Testing

When testing with `stack test`, you can filter the tests to run with:

```bash
$ stack test --ta '-p somethingTest'
$ stack test --ta '-p "test something 1"'
```
