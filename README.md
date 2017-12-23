# debug-hoed

A backend for the [`debug`](http://hackage.haskell.org/package/debug) package using ['Hoed'](http://hackage.haskell.org/package/Hoed) to generate the trace.

Using `debug-hoed` is very similar to using `debug`.
Turn on the `TemplateHaskell` and `ViewPatterns` extensions, import `Debug.Hoed`, indent your code and place it under a call to `debug`, e.g.:

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
main = runO $ putStrLn $ quicksort "haskell"
```

The call to `runO` starts a web browser to view the information recorded by Hoed, similar to the debug package.

## Noteworthy

- `Hoed` relies on the `Observable` class (which comes with a generic derivation) to collect the trace. Types without an `Observable` instance will fall back to the default one, which prints `<?>` for non observable types.

## Motivation

Why would you want to use `debug-hoed` instead of Hoed? The main reasons are:

- Hoed is compatible with laziness, so debugging will not alter the semantics of your program.
- Hoed can handle function arguments too, displaying them as maps. It can also observe partially applied functions.
- Hoed accomplishes this with less information, and therefore is potentially more efficient (but no one has performed serious benchmarks yet). 
