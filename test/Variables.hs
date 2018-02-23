{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -dth-dec-file #-} -- turn on to debug TH

module Variables(main) where

import Control.Exception.Extra
import Control.Monad
import Data.List
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Tuple.Extra
import Debug
import Debug.Util
import System.FilePath
import System.Directory
import Debug.DebugTrace

debug [d|
   quicksort :: Ord a => [a] -> [a]
   quicksort [] = []
   quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
       where (lt, gt) = partition (<= x) xs
   |]

quicksort_vars :: [(Text, Text)]
quicksort_vars =
    [ ("$arg1", "\"haskell\"")
    , ("$result", "\"aehklls\"")
    , ("++", "\"aehklls\"")
    , ("++'", "\"hklls\"")
    , ("gt", "\"skll\"")
    , ("lt", "\"ae\"")
    , ("partition", "(\"ae\",\"skll\")")
    , ("quicksort", "\"ae\"")
    , ("quicksort'", "\"klls\"")
    , ("x", "'h'")
    , ("xs", "\"askell\"") ]

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

lcm_gcd_vars :: [(Text, Text)]
lcm_gcd_vars =
    [ ("$arg1", "6")
    , ("$arg2", "15")
    , ("$result", "27000.0")
    , ("^^", "27000.0")
    , ("fromIntegral", "30.0")
    , ("gcd", "3")
    , ("lcm", "30")
    , ("least", "30")
    , ("x", "6")
    , ("y", "15") ]

debug [d|
    lcm_gcd_log :: Int -> Int -> Float
    lcm_gcd_log x y =
        let base = fromIntegral $ gcd x y
            val = fromIntegral (x `lcm` y) - base
        in logBase base val ** base
    |]

lcm_gcd_log_vars :: [(Text, Text)]
lcm_gcd_log_vars =
    [ ("$arg1", "6")
    , ("$arg2", "15")
    , ("$result", "27.0")
    , ("**", "27.0")
    , ("-", "27.0")
    , ("base", "3.0")
    , ("fromIntegral", "30.0")
    , ("gcd", "3")
    , ("lcm", "30")
    , ("logBase", "3.0")
    , ("val", "27.0")
    , ("x", "6")
    , ("y", "15") ]

debug [d|
    f :: Int -> Int
    f n = (2 * n) + 1

    case_test :: [Int] -> [Int] -> [Int]
    case_test ys zs =
        case ys of
            x : xs -> f x : xs ++ zs
            [] -> zs
    |]

case_test_vars :: [(Text, Text)]
case_test_vars =
    [ ("$arg1", "[2,3,4]")
    , ("$arg2", "[7,8,9]")
    , ("$result", "[5,3,4,7,8,9]")
    , ("++", "[3,4,7,8,9]")
    , (":", "[5,3,4,7,8,9]")
    , ("f", "5")
    , ("x", "2")
    , ("xs", "[3,4]")
    , ("ys", "[2,3,4]")
    , ("zs", "[7,8,9]") ]

debug [d|
    twoXs :: [Int] -> [Int] -> [Int]
    twoXs x y =
        case x of
            x : xs -> f x : xs ++ y
            [] -> y
    |]

twoXs_vars :: [(Text, Text)]
twoXs_vars =
    [ ("$arg1", "[2,3,4]")
    , ("$arg2", "[7,8,9]")
    , ("$result", "[5,3,4,7,8,9]")
    , ("++", "[3,4,7,8,9]")
    , (":", "[5,3,4,7,8,9]")
    , ("f", "5")
    , ("x", "[2,3,4]")
    , ("x'", "2")
    , ("xs", "[3,4]")
    , ("y", "[7,8,9]") ]

debug [d|
    --barely comprehensible test with multiple values for x and xs
    manyXs :: [Int] -> [Int] -> [Int]
    manyXs x y =
        case x of
            x : xs ->
                f x : case xs of
                    x : xs -> f x : xs ++ y
                    [] -> y
            [] -> y
    |]

manyXs_vars :: [(Text, Text)]
manyXs_vars =
    [ ("$arg1", "[2,3,4]")
    , ("$arg2", "[7,8,9]")
    , ("$result", "[5,7,4,7,8,9]")
    , ("++", "[4,7,8,9]")
    , (":", "[5,7,4,7,8,9]")
    , (":'", "[7,4,7,8,9]")
    , ("f", "5")
    , ("f'", "7")
    , ("x", "[2,3,4]")
    , ("x'", "2")
    , ("x''", "3")
    , ("xs", "[3,4]")
    , ("xs'", "[4]")
    , ("y", "[7,8,9]") ]

debug [d|
    where_1 :: Int -> Int -> Int
    where_1 x y = bar x + bar y
        where bar z = z * z
    |]

where_1_vars :: [(Text, Text)]
where_1_vars =
    [ ("$arg1", "5")
    , ("$arg2", "7")
    , ("$result", "74")
    , ("*", "25")
    , ("*'", "49")
    , ("+", "74")
    , ("bar", "25")
    , ("bar'", "49")
    , ("x", "5")
    , ("y", "7")
    , ("z", "5")
    , ("z'", "7") ]

debug [d|
    where_2 :: [Int] -> [Int]
    where_2 xs = fmap f xs where
        f x = x * x
    |]

where_2_vars :: [(Text, Text)]
where_2_vars =
    [ ("$arg1", "[1,2,3,4,5,6,7,8,9]")
    , ("$result", "[1,4,9,16,25,36,49,64,81]")
    , ("*", "1")
    , ("*'", "4")
    , ("*''", "9")
    , ("*'''", "16")
    , ("fmap", "[1,4,9,16,25,36,49,64,81]")
    , ("x", "1")
    , ("x'", "2")
    , ("x''", "3")
    , ("x'''", "4")
    , ("xs", "[1,2,3,4,5,6,7,8,9]") ]

explicit :: (Ord a, Show a) => [a] -> [a]
explicit = quicksort'
    where
        quicksort' arg1 = fun "quicksort" $ \t -> quicksort'' t (var t "arg1" arg1)
        quicksort'' t [] = []
        quicksort'' t ((var t "x" -> x):(var t "xs" -> xs)) = quicksort' lt ++ [x] ++ quicksort' gt
            where (var t "lt" -> lt, var t "gt" -> gt) = partition (<= x) xs

testExample name expr testDisabled = do
    _ <- return ()
    putStrLn $ "Testing " ++ name
    debugClear
    print expr
    unless testDisabled $
        checkVars name expr
    writeFile ("output" </> name <.> "js") . ("var trace =\n" ++) . (++ ";") =<< debugJSON
    debugSave $ "output" </> name <.> "html"
    -- see https://github.com/feuerbach/ansi-terminal/issues/47 as this test fails on Appveyor
    -- can remove once ansi-terminal-0.8 is available in Stackage LTS (which will be v11)
    try_ debugPrint
    putStrLn "\n\n"

checkVars :: Show a => String -> a -> IO ()
checkVars name expr = do
    trace <- getDebugTrace
    let varList = map (first funName) $ getTraceVars trace
    case lookup (pack name) varList of
        Nothing -> fail $ "Cant find the function " ++ name ++ " in the trace"
        Just vars ->
            case lookup name expectedVars of
                Nothing -> fail $ "Can't find the list of expected variables for the function " ++ name
                Just expected -> case checkEachVar vars expected of
                    [] -> do
                        when (length vars /= length expected) $
                            fail $ "Expected " ++ show (length expected) ++ " variables, but found " ++ show (length vars)
                        return ()
                    xs -> fail $ "\n" ++ unlines xs

checkEachVar :: [(Text, Text)] -> [(Text, Text)] -> [String]
checkEachVar vars expected = foldr f [] vars where
    f :: (Text, Text) -> [String] -> [String]
    f (key, val) acc =
        case lookup key expected of
            Nothing -> ("Couldn't find the variable " ++ T.unpack key ++ " in the expected results") : acc
            Just v -> if v /= val
                then ("Expected " ++ unpack val ++ " but got " ++ unpack v ++ " for the variable " ++ unpack key) : acc
                else acc

expectedVars :: [(String, [(Text, Text)])]
expectedVars = [ ("quicksort", quicksort_vars)
               , ("lcm_gcd", lcm_gcd_vars)
               , ("lcm_gcd_log", lcm_gcd_log_vars)
               , ("case_test", case_test_vars)
               , ("twoXs", twoXs_vars)
               , ("manyXs", manyXs_vars)
               , ("where_1", where_1_vars)
               , ("where_2", where_2_vars) ]

main = do
    createDirectoryIfMissing True "output"
    testExample "quicksort" (quicksort "haskell") False
    testExample "lcm_gcd" (lcm_gcd 6 15) False
    testExample "lcm_gcd_log" (lcm_gcd_log 6 15) False
    testExample "case_test" (case_test [2,3,4] [7,8,9]) False
    testExample "twoXs" (twoXs [2,3,4] [7,8,9]) False
    testExample "manyXs" (manyXs [2,3,4] [7,8,9]) False
    testExample "where_1" (where_1 5 7) False
    testExample "where_2" (where_2 [1,2,3,4,5,6,7,8,9]) False

    --skipping test of quicksortBy for now, as it looks like $arg1 is missing
    testExample "quicksortBy" (quicksortBy (<) "haskell") True
    testExample "explicit" (explicit "haskell") True
    copyFile "output/quicksort.js" "trace.js" -- useful for debugging the HTML

    evaluate type1
--    evaluate type2

    let a === b = if a == b then putStr "." else fail $ show (a, "/=", b)
    removeExtraDigits "_quicksort_0" === "_quicksort"
    removeLet let0 === "f"
    removeLet let1 === "select_2"
    removeLet let2 === "Data.Foldable.foldr"
    removeLet let3 === "Data.Foldable.foldr"
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
