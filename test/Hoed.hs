{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE ExtendedDefaultRules, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Hoed where

import qualified Data.ByteString.Lazy as B
import Debug.Record hiding (getDebugTrace)
import Debug.Hoed
import System.Exit
import System.Process

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

debug [d|
    foo :: m a -> m a
    foo = id
    |]

main = do
    _ <- return ()
    trace <- getDebugTrace defaultHoedOptions $ putStrLn$ quicksort (<) "haskell"
    debugPrintTrace trace
    B.writeFile "hoed.json" $ debugJSONTrace trace
    exitWith =<< system "stack exec diff hoed.json test/ref/hoed.json"
    debugSaveTrace "trace.html" trace
