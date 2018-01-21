# debug-hoed

A backend for the [`debug`](http://hackage.haskell.org/package/debug) package using [`Hoed`](http://hackage.haskell.org/package/Hoed) to generate the trace.

Using `debug-hoed` is very similar to using `debug`.
Turn on the `TemplateHaskell`, `PartialTypeSignatures` and `ViewPatterns` extensions, import `Debug.Hoed`, indent your code and place it under a call to `debug`, e.g.:

```haskell
{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module QuickSort(quicksort) where
import Data.List
import Debug.Hoed

debug [d|
   quicksort :: Ord a => [a] -> [a]
   quicksort [] = []
   quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
       where (lt, gt) = partition (<= x) xs
   |]
```

We can now run our debugger with:

```haskell
main :: IO ()
main = debugView $ putStrLn $ quicksort "haskell"
```

The call to `debugRun` starts a web browser to view the information recorded by Hoed, similar to the debug package.

## Requirements

- Polymorphic functions must have type signatures, otherwise GHC will fail to infer an unambiguous type when annotated for debugging.
- Polymorphic numeric literals must be annotated with a specific type, for the same reason as above
- Types under observation must be given an 'Observable' instance, otherwise they will fall back to the default one, which prints `<?>` for non observable types. 

The 'Observable' class is derivable for 'Generic' types, and the `debug'` TH wrapper can optionally append `deriving anyclass Observable` to Generic types automatically. 

## Motivation

Why would you want to use `debug-hoed` instead of `debug`? The main reasons are:

- `debug-hoed` is compatible with laziness, so debugging will not alter the semantics of your program.
- `debug-hoed` can handle function arguments too, displaying them as maps. It can also observe partially applied functions.
- `debug-hoed` provides call stacks. 
