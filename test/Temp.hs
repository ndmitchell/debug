{-# OPTIONS -F -pgmF debug-pp #-}

module Temp where

l1, l2 :: [Int]
l1 = [2, 3, 4]
l2 = [7, 8, 9]

--copied lcm_gcd from Main.hs:
lg :: (Integral a) => a -> a -> Double
lg x y =
    let least = lcm x y
    in fromIntegral least ^^ gcd x y

where_1 :: Int -> Int -> Int
where_1 x y = bar x + bar y
    where bar z = z * z

where_2 :: Int -> Int
where_2 y = f y where
    f x = x * x

where_3 :: [Int] -> [Int]
where_3 xs = fmap f xs where
    f x = x * x

