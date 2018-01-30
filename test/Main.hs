
module Main(main) where

import qualified Hoed
import qualified Variables

main :: IO ()
main = do
    Variables.main
    Hoed.main
