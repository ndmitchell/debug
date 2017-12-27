# debug-pp

A preprocessor for streamlining the `debug` instrumentation of a module or a package.

Description
---------------

`debug-pp` is a Haskell source preprocessor that performs the steps a programmer would need to follow in order to debug a module with the `debug` package. That is:
* append an import for the `Debug` module, and 
* wrap the body in a `debug` splice using a TH declaration quasiquote. 

Usage
--------
```
usage: debug-pp [FILENAME] [SOURCE] [DEST]
Instrument Haskell module for debugging from SOURCE (derived from FILENAME) and write
standard Haskell to DEST.
If no FILENAME, use SOURCE as the original name.
If no DEST or if DEST is `-', write to standard output.
If no SOURCE or if SOURCE is `-', read standard input.
```
To instrument a module, add the following pragma to the top of the file:
```
{-# OPTIONS -F -pgmF debug-pp #-}
```

To instrument an entire program, add the following line to your stack descriptor, or if you don't use stack, to your cabal descriptor:
```
ghc-options: -F -pgmF debug-pp
```

In both cases you will also need to modify your Cabal descriptor in order to:
* add a dependency on the `debug` package, and
* add a build tool depends on `debug-pp` (required Cabal 2.0) :
```
Library
  ...
  build-tool-depends: debug-pp:debug-pp
```
