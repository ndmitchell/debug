{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Temp where

import Debug

-- foo x = case x of
--   x:xs -> ...
debug [d|

    f :: Int -> Int
    f = (2*)

    oneX :: [Int] -> [Int] -> [Int]
    oneX ys zs =
        case ys of
            x : xs -> f x : xs ++ zs
            [] -> zs

    manyXs :: [Int] -> [Int] -> [Int]
    manyXs x y =
        case x of
            x : xs -> f x : xs ++ y
            [] -> y

    --copied lcm_gcd from Main.hs:
    lg :: (Integral a) => a -> a -> Double
    lg x y =
        let least = lcm x y
        in fromIntegral least ^^ gcd x y
    |]

l1, l2 :: [Int]
l1 = [1, 2, 3, 4, 5]
l2 = [6, 7, 8, 9, 10]

