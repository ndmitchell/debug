{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- {-# OPTIONS_GHC -dth-dec-file #-} -- turn on to debug TH

module Variables(main) where

import Debug
import Debug.Util
import Data.List
import Control.Exception.Extra
import System.Directory
import System.FilePath


debug [d|
   quicksort :: Ord a => [a] -> [a]
   quicksort [] = []
   quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
       where (lt, gt) = partition (<= x) xs
   |]

debug [d|
    quicksortBy :: (a -> a -> Bool) -> [a] -> [a]
    quicksortBy op [] = []
    quicksortBy op (x:xs) = quicksortBy op lt ++ [x] ++ quicksortBy op gt
        where (lt, gt) = partitionBy (op x) xs

    partitionBy  :: (a -> Bool) -> [a] -> ([a],[a])
    {-# INLINE partitionBy #-}
    partitionBy p xs = foldr (select p) ([],[]) xs

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
    lcm_gcd :: (Integral a) => a -> a -> Double
    lcm_gcd x y =
        let least = lcm x y
        in fromIntegral least ^^ gcd x y
    |]
--  expected:
--      x, $arg1 = 6
--      y, $arg2 = 15
--      $result = 27000.0
--      ^^ = 27000.0
--      fromIntegral = 30.0
--      gcd = <function>
--      gcd' = 3
--      lcm = <function>
--      lcm' = 30
--      least = 30

debug [d|
    lcm_gcd_log :: Int -> Int -> Float
    lcm_gcd_log x y =
        let base = fromIntegral $ gcd x y
            val = fromIntegral (x `lcm` y) - base
        in logBase base val ** base
    |]
--  expected:
--      $arg1, x = 6
--      $arg2, y = 15
--      $result = 27.0
--      ** = 27.0
--      - = 27.0
--      base = 3.0
--      fromIntegral = 30.0
--      gcd = <function>
--      gcd' = 3
--      lcm = 30
--      logBase = <function>
--      logBase' = 3.0
--      val = 27.0

debug [d|
    f :: Int -> Int
    f = (2*)

    case_test :: [Int] -> [Int] -> [Int]
    case_test ys zs =
        case ys of
            x : xs -> f x : xs ++ zs
            [] -> zs
    |]
--  expected:
--      ys, $arg1 = [1,2,3]
--      zs, $arg2 = [4,5,6]
--      x = 1
--      xs = [2,3]
--      f = 2
--      ++ = [2,3,4,5,6]
--      : = [2,2,3,4,5,6]
--      $result = [2,2,3,4,5,6]

explicit :: (Ord a, Show a) => [a] -> [a]
explicit = quicksort'
    where
        quicksort' arg1 = fun "quicksort" $ \t -> quicksort'' t (var t "arg1" arg1)
        quicksort'' t [] = []
        quicksort'' t ((var t "x" -> x):(var t "xs" -> xs)) = quicksort' lt ++ [x] ++ quicksort' gt
            where (var t "lt" -> lt, var t "gt" -> gt) = partition (<= x) xs


example name expr = do
    _ <- return ()
    putStrLn $ "Testing " ++ name
    debugClear
    print expr
    writeFile ("output" </> name <.> "js") . ("var trace =\n" ++) . (++ ";") =<< debugJSON
    debugSave $ "output" </> name <.> "html"
    -- see https://github.com/feuerbach/ansi-terminal/issues/47 as this test fails on Appveyor
    -- can remove once ansi-terminal-0.8 is available in Stackage LTS (which will be v11)
    debugPrint
    putStrLn "\n\n"

main = do
    createDirectoryIfMissing True "output"
    example "quicksort" $ quicksort "haskell"
    example "quicksortBy" $ quicksortBy (<) "haskell"
    example "lcm_gcd" $ lcm_gcd 6 15
    example "lcm_gcd_log" $ lcm_gcd_log 6 15
    example "case_test" $ case_test [1, 2, 3] [4, 5, 6]
    example "explicit" $ explicit "haskell"
    copyFile "output/quicksort.js" "trace.js" -- useful for debugging the HTML

    evaluate type1
--    evaluate type2

    let a === b = if a == b then putStr "." else fail $ show (a, "/=", b)
    removeExtraDigits "_quicksort_0" === "_quicksort"
    removeLet let0 === "f"
    removeLet let1 === "select_2'"
    removeLet let2 === "Data.Foldable.foldr'"
    removeLet let3 === "Data.Foldable.foldr''"
    mkLegalInfixVar "+" === "plus"
    mkLegalInfixVar "<!>" === "lt_bang_gt"
    mkLegalInfixVar "`lcd`" === "lcd"
    mkLegalInfixVar "abc" === "abc"

    putStrLn " done"

let0, let1, let2 :: String
let0 = "f"
let1 = "let (Debug.DebugTrace.var tag_0 \"_select_0\" -> _select_0_1) = select_2 p_3"
let2 = "let (Debug.DebugTrace.var tag_0 \"_foldr\" -> _foldr_1) = Data.Foldable.foldr \
        \(let (Debug.DebugTrace.var tag_0 \"_select_0\" -> _select_0_2) = select_3 p_4 \
        \in _select_0_2)"
let3 = "let (Debug.DebugTrace.var tag_0 \"_foldr'\" -> _foldr'_1) = (let (Debug.DebugTrace.var \tag_0 \
       \\"_foldr\" -> _foldr_2) = Data.Foldable.foldr (let (Debug.DebugTrace.var tag_0 \"_select_0\" \
       \-> _select_0_3) = select_4 p_5 in _select_0_3) in _foldr_2) ([], []) \
       \in _foldr'_1"
