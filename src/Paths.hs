-- | Fake cabal module for local building

module Paths_debug(getDataFileName) where

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return
