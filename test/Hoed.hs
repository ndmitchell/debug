{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE ExtendedDefaultRules, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -dth-dec-file #-} -- turn on to debug TH
module Hoed(main) where

import Control.Exception.Extra
import Debug.Hoed
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Util

#if __GLASGOW_HASKELL__ >= 802
debug [d|
    quicksort :: (a -> a -> Bool) -> [a] -> [a]
    quicksort op [] = []
    quicksort op (x:xs) = quicksort op lt ++ [x] ++ quicksort op gt
        where (lt, gt) = partition (op x) xs

    partition               :: (a -> Bool) -> [a] -> ([a],[a])
    {-# INLINE partition #-}
    partition p = foldr (select p) ([],[])

    select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
    select p x ~(ts,fs) | p x       = (x:ts,fs)
                        | otherwise = (ts, x:fs)
  |]
#endif

debug [d|
    foo :: m a -> m a
    foo = id

    listcomp, listmap :: Num a => a -> a
    listcomp y = sum [x*2 | x <- [1..y]]
    listmap y = sum $ map (\x -> 1+x*2) [1..y]
  |]

main :: IO ()
main = do
    trace <- getDebugTrace defaultHoedOptions $ do
#if __GLASGOW_HASKELL__ >= 802
      print (quicksort (<) "haskell")
#endif
      print (listmap (3::Int))
      print (listcomp (3::Int))
    -- see https://github.com/feuerbach/ansi-terminal/issues/47 as this test fails on Appveyor
    -- can remove once ansi-terminal-0.8 is available in Stackage LTS (which will be v11)
    try_ $ debugPrintTrace trace
    B.writeFile "hoed.json" $ encode trace
#if __GLASGOW_HASKELL__ >= 802
    Just refTrace <- decode <$> B.readFile "test/ref/hoed.json"
#else
    Just refTrace <- decode <$> B.readFile "test/ref/hoed80.json"
#endif
    unless (equivalentTrace trace refTrace) $
      error "Trace does not match the reference value"
    print (foo ['c'])
