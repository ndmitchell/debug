{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Control.Monad
import Data.Char
import Data.List
import Data.Yaml
import Data.Yaml.Config
import GHC.Generics
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Text.Printf

usage :: String -> String
usage progName = unlines [
  "usage: " ++ progName ++
  " [FILENAME] [SOURCE] [DEST]",
  "Instrument Haskell module for debugging from SOURCE (derived from FILENAME) and write",
  "standard Haskell to DEST.",
  "If no FILENAME, use SOURCE as the original name.",
  "If no DEST or if DEST is `-', write to standard output.",
  "If no SOURCE or if SOURCE is `-', read standard input."
  ]

data Config = Config
  { excluded :: [String]
  } deriving (FromJSON, ToJSON, Generic, Show)

defaultConfig = Config []

readConfig :: IO Config
readConfig = do
  cwd <- getCurrentDirectory
  home <- getHomeDirectory
  system <- getXdgDirectory XdgConfig "debug-pp"
  from <- filterM doesFileExist $
        [d </> ".debug-pp.yaml" | d <- reverse (ancestors cwd)] ++
        [ system </> "config.yaml"
        , home </> ".debug-pp.yaml"]
  case from of
    [] -> return defaultConfig
    _  -> loadYamlSettings from [] ignoreEnv
  where
    ancestors = map joinPath . tail . inits . splitPath

defConfig = unlines
  ["# debug-pp configuration file"
  ,"# ==========================="
  ,""
  ,"# List of Haskell module names to exclude from instrumentation:"
  ,"excluded: []"
  ]

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  (orig, inp, out) <- case args of
    ["--defaults"] -> do
      putStrLn defConfig
      exitSuccess
    ["--help"] -> do
      putStrLn $ usage progName
      exitSuccess
    []     -> return ("input",Nothing,Nothing)
    [i]    -> return (i, Just i, Nothing)
    [i,o]  -> return (i, Just i, Just o)
    [orig,i,o] -> return (orig, Just i, Just o)
    _ -> do
      putStrLn $ usage progName
      error "Unrecognized set of command line arguments"
  hIn  <- maybe (return stdin)  (`openFile` ReadMode) inp
  hOut <- maybe (return stdout) (`openFile` WriteMode) out
  contents <- hGetContents hIn
  config <- readConfig
  hPutStr hOut $ instrument config contents
  unless (hOut == stdout) $ hClose hOut

instrument Config{..} contents
  | name `elem` excluded = contents
  | otherwise = unlines [top', modules', body']
  where
    (top,name,modules,body) = parseModule contents
    modules' = unlines $ modules ++ ["import qualified Debug"]
    top' =
      unlines
        $ "{-# LANGUAGE TemplateHaskell #-}"
        : "{-# LANGUAGE PartialTypeSignatures #-}"
        : "{-# LANGUAGE ViewPatterns #-}"
        : "{-# OPTIONS -Wno-partial-type-signatures #-}"
        : top
    body' = unlines $ "Debug.debug [d|" : map indent (body ++ ["  |]"])

parseModule contents = (top, name, modules, body)
  where
    isImportLine = ("import " `isPrefixOf`)
    (top, rest) = break isImportLine (lines contents)
    (reverse -> body0, reverse -> modules0) =
      break (isImportLine . fst) (reverse $ annotateBlockComments rest)
    nameLine =
      head
        [l | (l, False) <- annotateBlockComments top, "module " `isPrefixOf` l]
    name = takeWhile (\x -> not (isSpace x || x == '(')) $ drop 7 nameLine
    body = map fst $ dropWhile snd body0
    modules = map fst modules0 ++ map fst (takeWhile snd body0)

indent it@('#':_) = it
indent other = "  " ++ other

annotateBlockComments :: [String] -> [(String, Bool)]
annotateBlockComments = annotateBlockComments' False
annotateBlockComments' _ [] = []
annotateBlockComments' False (l:rest)
    | "{-" `isInfixOf` l = (l,True) : annotateBlockComments' True rest
    | otherwise = (l,False) : annotateBlockComments' False rest
annotateBlockComments' True (l:rest) =
  (l,True) : annotateBlockComments' (not $ "-}" `isInfixOf` l) rest
