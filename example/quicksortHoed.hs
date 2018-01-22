{-# LANGUAGE ExtendedDefaultRules, FlexibleContexts, TemplateHaskell, ViewPatterns, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module QuickSortHoed(main, quicksort) where
import Debug.Hoed
import Data.List

debug [d|
   quicksort :: Ord a => [a] -> [a]
   quicksort [] = []
   quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
       where (lt, gt) = partition (<= x) xs
   |]

main = debugRun $ print $ quicksort "haskell"
