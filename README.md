# Haskell Debugger [![Hackage version](https://img.shields.io/hackage/v/debug.svg?label=Hackage)](https://hackage.haskell.org/package/debug) [![Stackage version](https://www.stackage.org/package/debug/badge/lts?label=Stackage)](https://www.stackage.org/package/debug) [![Linux Build Status](https://img.shields.io/travis/ndmitchell/debug.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/debug) [![Windows Build Status](https://img.shields.io/appveyor/ci/ndmitchell/debug.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/debug)

A library for debugging Haskell programs. To use, take the functions that you are interested in debugging, e.g.:

```haskell
module QuickSort(quicksort) where
import Data.List

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
    where (lt, gt) = partition (<= x) xs
```

Turn on the `TemplateHaskell` and `ViewPatterns` extensions, import `Debug`, indent your code and place it under a call to `debug`, e.g.:

```haskell
{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module QuickSort(quicksort) where
import Data.List
import Debug

debug [d|
   quicksort :: Ord a => [a] -> [a]
   quicksort [] = []
   quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
       where (lt, gt) = partition (<= x) xs
   |]
```

We can now run our debugger with:

```console
$ ghci QuickSort.hs
GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling QuickSort        ( QuickSort.hs, interpreted )
Ok, 1 module loaded.
*QuickSort> quicksort "haskell"
"aehklls"
*QuickSort> debugView
```

The call to `debugView` starts a web browser to view the recorded information, looking something like:

![Debug view output](debug.png)
