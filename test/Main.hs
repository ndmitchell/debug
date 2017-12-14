{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -dth-dec-file #-} -- turn on to debug TH

module Main(main) where

import Debug
import Debug.Record

debug [d|
    quicksort :: (a -> a -> Bool) -> [a] -> [a]  
    quicksort op [] = []  
    quicksort op (x:xs) = quicksort op lt ++ [x] ++ quicksort op gt
        where (lt, gt) = partition (op x) xs

    partition               :: (a -> Bool) -> [a] -> ([a],[a])
    {-# INLINE partition #-}
    partition p xs = foldr (select p) ([],[]) xs

    select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
    select p x ~(ts,fs) | p x       = (x:ts,fs)
                        | otherwise = (ts, x:fs)
    |]

quicksort' :: (Ord a, Show a) => [a] -> [a]
quicksort' arg1 = fun "quicksort" $ \t -> quicksort'' t (var t "arg1" arg1)
quicksort'' t [] = []
quicksort'' t ((var t "x" -> x):(var t "xs" -> xs)) = quicksort' lt ++ [x] ++ quicksort' gt
    where (var t "lt" -> lt, var t "gt" -> gt) = partition (<= x) xs

main = do
    _ <- return ()
    debugClear
    print $ quicksort (<) "neil"
    print $ quicksort' "neil"
    debugConsole
    writeFile "trace.js" =<< debugJS
