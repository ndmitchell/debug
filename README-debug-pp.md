[![Travis Build Status](https://travis-ci.org/pepeiborra/debug-pp.svg)](https://travis-ci.org/pepeiborra/debug-pp)
[![Hackage](https://img.shields.io/hackage/v/debug-pp.svg)](https://hackage.haskell.org/package/debug-pp)
[![Stackage Nightly](http://stackage.org/package/debug-pp/badge/nightly)](http://stackage.org/nightly/package/debug-pp)
# debug-pp

A preprocessor for streamlining the `debug` instrumentation of a module or a package.

Description
---------------

`debug-pp` is a Haskell source preprocessor that performs the steps a programmer would need to follow in order to debug a module with the [`debug`](http://hackage.haskell.org/package/debug) package. That is:
* append an import for the `Debug` module, and 
* wrap the body in a `debug` splice using a TH declaration quasiquote. 
* Add the required GHC extensions.

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
* add a dependency on either the `debug` package or the `debug-hoed` package, and
* add a build tool depends on `debug-pp` (required Cabal 2.0) :
```
Library
  ...
  build-tool-depends: debug-pp:debug-pp
```
Configuration
-------------

The tool is customizable to some extent. It tries to find a config file in the
following order:

2. `.debug-pp.yaml` in the current directory (useful for per-directory
   settings)
3. `.debug-pp.yaml` in the nearest ancestor directory (useful for
   per-project settings)
4. `debug-pp/config.yaml` in the platform’s configuration directory
   (on Windows, it is %APPDATA%, elsewhere it defaults to `~/.config` and
   can be overridden by the `XDG_CONFIG_HOME` environment variable;
   useful for user-wide settings)
5. `.debug-pp.yaml` in your home directory (useful for user-wide
   settings)
6. The default settings.

Use `debug-pp --defaults > .debug-pp.yaml` to dump a
well-documented default configuration to a file, this way you can get started
quickly.

The configuration options include:
* Exclude modules by name.
* Instrument the `main` function to launch a web browser upon completion.
* Select which language extensions are included automatically.
* Configure the `debug-hoed` instrumentation to include e.g. deriving `Generic` and `Observable` instances


Motivation
-------------

Debugging individual functions is often impractical due to the declaration groups [restriction](http://ghc.readthedocs.io/en/8.0.1/glasgow_exts.html#ghc-flag--XTemplateHaskellQuotes) of Template Haskell. Therefore, it's often easier to debug an entire module.

However, error messages are much worse for TH quoted code. Errors like `Not in scope: data constructor Foo in line X` become `Not in scope: Foo, in the TH quotation ENTIRE MODULE HERE`. 

Personally, quoting/unquoting the module by hand is annoying. Having a preprocessor do this for me is convenient, and can be easily tied to a Cabal flag or a stack command line arg.
