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

Turn on the `TemplateHaskell`, `ViewPatterns` and `PartialTypeSignatures` extensions, import `Debug`, indent your code and place it under a call to `debug`, e.g.:

```haskell
{-# LANGUAGE TemplateHaskell, ViewPatterns, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
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

You can look play with the example results for various examples:

* [`quicksort "haskell"`](https://ci.appveyor.com/api/projects/ndmitchell/debug/artifacts/quicksort.html) as above.
* [`quicksortBy (<) "haskell"`](https://ci.appveyor.com/api/projects/ndmitchell/debug/artifacts/quicksortBy.html), like `quicksort` but using a comparison function and including a trace of `partition` itself.
* [`lcm_gcd 6 15`](https://ci.appveyor.com/api/projects/ndmitchell/debug/artifacts/lcm_gcd.html), computing `lcm 6 15 ^^ gcd 6 15`.

## Notes

Calling the debugged function inside GHCi records the results for viewing inside the UI.
The function can be called multiple times with different parameters, and the results of each
individual run can be selected inside the UI.

You can create multiple `debug [d|...]` blocks inside a module and you can also put more than one
function inside a single block.

A function being debugged can refer to another function also being debugged, but due to a limitation
of Template Haskell, the definition of the function being called must occur above the point of its
reference in the source module.

## Limitations

This tool is quite new, so it has both limitations, places it is incomplete and bugs. Some notable issues:

* It calls `show` on all the values in encounters, meaning they must all have a `Show` instance (it defines a global `Show` instance which should get used as a fallback), and they will be fully evaluated. If your program relies on laziness it probably won't work.
* It doesn't really understand shadowed variables, so it will work, but the debug results will be lower quality.
* For function values it won't give you a whole lot of information.

## Alternatives

For practical alternatives for debugging Haskell programs you may wish to consider:

* [GHCi debugger](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-debugger), simple imperative-style debugger in which you can stop a running computation in order to examine the values of variables. The debugger is integrated into GHCi. Robust, reliable, somewhat difficult to use.
* [Hood](https://hackage.haskell.org/package/hood) and [Hoed](https://hackage.haskell.org/package/Hoed), a value-based observational debugger with a difficult user interface, deals well with laziness.
* [Hat](https://hackage.haskell.org/package/hat), good ideas, but I've never got it working.

Compared to the above, `debug` stresses simplicity of integration and user experience.

## FAQ

### Q: `debugView` fails talking about Wine?

A: If you get `wine: invalid directory "/home/f/.wine" in WINEPREFIX: not an absolute path` when running `debugView` that means `xdg-open` is handled by [Wine](https://www.winehq.org/). Fix that and it will work once more.

### Q: `debugView` fails with "error: Variable not in scope: debugView" ?

A: Explicitly load the Debug module in GHCi via `:m + Debug`
