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
  , instrumentMain :: Bool
  , verbose :: Bool
  } deriving (FromJSON, ToJSON, Generic, Show)

defaultConfig = Config [] True False

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
  ,"# List of Haskell module names to exclude from instrumentation"
  ,"excluded: []"
  , ""
  , "# If true then insert a call to debugRun in the main function."
  , "instrumentMain: true"
  , ""
  , "# If true, print a line for every instrumented module."
  , "verbose: false"
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
  config@Config{..} <- readConfig
  hPutStr hOut $ instrument config contents
  unless (hOut == stdout) $ hClose hOut
  when verbose $
    putStrLn $ "[debug-pp] Instrumented " ++ orig

instrument Config{..} contents
  | name `elem` excluded = contents
  | otherwise = unlines [top', modules', body'']
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
    body'  = map (if instrumentMain then instrumentMainFunction else id) body
    body'' = unlines $ "Debug.debug [d|" : map indent (body' ++ ["  |]"])

instrumentMainFunction l
  | ('m':'a':'i':'n':rest) <- l
  , ('=':rest') <- dropWhile isSpace rest
  , not ("debugRun" `isPrefixOf` rest') = "main = Debug.debugRun $ " ++ rest'
  | otherwise = l

parseModule contents = (map fst top, name, modules, body)
  where
    contents' = annotateBlockComments (lines contents)
    isImportLine = ("import " `isPrefixOf`)
    (top, rest) = break (\(l, insideComment) -> not insideComment && isImportLine l) contents'
    (reverse -> body0, reverse -> modules0) = break (\(l,insideComment) -> not insideComment && isImportLine l) (reverse rest)
    nameLine =
      head $
      [l | (l, False) <- top, "module " `isPrefixOf` l] ++
      ["Main"]
    name = takeWhile (\x -> not (isSpace x || x == '(')) $ drop 7 nameLine
    body = map fst $ dropWhile snd body0
    modules = map fst modules0 ++ map fst (takeWhile snd body0)

indent it@('#':_) = it
indent other = "  " ++ other

-- Annotate every line with True if its inside the span of a block comment.
-- @
--   {- LANGUAGE foo -}     -- False
--   This is not inside {-  -- False
--   but this is -}         -- True
annotateBlockComments :: [String] -> [(String, Bool)]
annotateBlockComments = annotateBlockComments' False
annotateBlockComments' _ [] = []
annotateBlockComments' False (l:rest) = (l,False) : annotateBlockComments' (startsBlockComment l) rest
annotateBlockComments' True  (l:rest) = (l,True) : annotateBlockComments' (not $ endsBlockComment l) rest

startsBlockComment line
    | Just l' <- dropUntilIncluding "{-" line = not $ endsBlockComment l'
    | otherwise = False

endsBlockComment line
    | Just l' <- dropUntilIncluding "-}" line = not $ startsBlockComment l'
    | otherwise = False

dropUntilIncluding needle haystack
  | [] <- haystack = Nothing
  | Just x <- stripPrefix needle haystack = Just x
  | x:rest <- haystack = (x:) <$> dropUntilIncluding needle rest
