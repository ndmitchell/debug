{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- {-# OPTIONS_GHC -dth-dec-file #-} -- turn on to debug TH

module Main(main) where

import Debug
import Debug.Record
import Control.Exception.Extra

debug [d|
    quicksort :: (a -> a -> Bool) -> [a] -> [a]
    quicksort op [] = []
    quicksort op (x:xs) = quicksort op lt ++ [x] ++ quicksort op gt
        where (lt, gt) = partition (op x) xs

    partition  :: (a -> Bool) -> [a] -> ([a],[a])
    {-# INLINE partition #-}
    partition p xs = foldr (select p) ([],[]) xs

    select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
    select p x ~(ts,fs) | p x       = (x:ts,fs)
                        | otherwise = (ts, x:fs)
    |]

debug [d|
    type1 :: m a -> m a
    type1 = id

--    type2 :: Int -> m Int
--    type2 _ = undefined
    |]

debug [d|    
    g :: (Integral a) => a -> a -> Double   -- g 6 15 = 2700.0
    g x y = 
        let least = lcm x y 
        in fromIntegral least ^^ gcd x y  
    |]
--  expected: 
--      x, $arg1 = 6
--      y, $arg2 = 15
--      least = 30
--      fromIntegral = 30.0
--      lcm = 3
--      (^^), $result = 2700

quicksort' :: (Ord a, Show a) => [a] -> [a]
quicksort' arg1 = fun "quicksort" $ \t -> quicksort'' t (var t "arg1" arg1)
quicksort'' t [] = []
quicksort'' t ((var t "x" -> x):(var t "xs" -> xs)) = quicksort' lt ++ [x] ++ quicksort' gt
    where (var t "lt" -> lt, var t "gt" -> gt) = partition (<= x) xs

main = do
    _ <- return ()
    debugClear
    print $ quicksort (<) "haskell"
    print $ g 6 15
    -- see https://github.com/feuerbach/ansi-terminal/issues/47 as this test fails on Appveyor
    try_ debugPrint
    writeFile "trace.js" . ("var trace =\n" ++) . (++ ";") =<< debugJSON
    debugSave "trace.html"
    evaluate type1
--    evaluate type2
    print $ quicksort' "haskell"

    --the following tests can be an hspec tests...
    putStrLn $ removeExtraDigits "_quicksort_0"
    
    putStrLn $ "let0: " ++ removeLet let0   -- f 
    putStrLn $ "let1: " ++ removeLet let1   -- select_2'
    putStrLn $ "let2: " ++ removeLet let2   -- Data.Foldable.foldr'
    putStrLn $ "let3: " ++ removeLet let3   -- Data.Foldable.foldr''
 
let0, let1, let2 :: String
let0 = "f"
let1 = "let (Debug.Record.var tag_0 \"_select_0\" -> _select_0_1) = select_2 p_3"
let2 = "let (Debug.Record.var tag_0 \"_foldr\" -> _foldr_1) = Data.Foldable.foldr \ 
        \(let (Debug.Record.var tag_0 \"_select_0\" -> _select_0_2) = select_3 p_4 \
        \in _select_0_2)"
let3 = "let (Debug.Record.var tag_0 \"_foldr'\" -> _foldr'_1) = (let (Debug.Record.var \tag_0 \
       \\"_foldr\" -> _foldr_2) = Data.Foldable.foldr (let (Debug.Record.var tag_0 \"_select_0\" \
       \-> _select_0_3) = select_4 p_5 in _select_0_3) in _foldr_2) ([], []) \
       \in _foldr'_1"