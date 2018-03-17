{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Dodgy Show instance, useful for debugging
{-# OPTIONS_GHC -Wno-deprecations #-} -- Dodgy Show instance, useful for debugging

-- | Module for recording and manipulating debug traces. For most users, the
--   @TemplateHaskell@ helpers in "Debug" should be sufficient.
module Debug.DebugTrace(
    -- * Debug traces
    DebugTrace(..),
    Function(..),
    CallData(..),
    -- * Viewing the trace
    debugPrintTrace,
    debugJSONTrace,
    debugViewTrace,
    debugSaveTrace,
    getTraceVars
    ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Text
import Data.Aeson.Types
import Data.Char
import Data.Hashable
import Data.List.Extra
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Text.Read as T
import Data.Tuple.Extra
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector as V
import GHC.Generics
import System.IO
import System.Directory
import Text.Show.Functions() -- Make sure the Show for functions instance exists
import Web.Browser
import Paths_debug
import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>), (<>))
import Prelude


-- | Metadata about a function, used to drive the HTML view.
data Function = Function
    {funName :: Text -- ^ Function name
    ,funSource :: Text -- ^ Function source, using @\n@ to break lines
    ,funArguments :: [Text] -- ^ Variables for the arguments to the function
    ,funResult :: Text -- ^ Variable for the result of the function
    }
    deriving (Eq,Generic,Ord,Show)

instance Hashable Function
instance NFData Function

-- | Along with the function metatdata, get a list of the variable names and string values from the trace
getTraceVars :: DebugTrace -> [(Function, [(Text, Text)])]
getTraceVars DebugTrace{..} =
    let lookupFun = (V.fromList functions V.!)
        lookupVar = (V.fromList variables V.!)
    in [ (lookupFun callFunctionId, map (second lookupVar) callVals)
       | CallData{..} <- calls ]

-- | Print information about the observed function calls to 'stdout',
--   in a human-readable format.
debugPrintTrace :: DebugTrace -> IO ()
debugPrintTrace trace@DebugTrace{..} = do
    let concs = getTraceVars trace
    let docs = map call $ nubOrd $ reverse concs
    putDoc (vcat docs <> hardline)
    where
          call :: (Function, [(Text, Text)]) -> Doc
          call (f, vs) =
                   let ass = vs
                       hdr = bold $ header ass f
                   in hang 5 $ hdr <$$> body ass

          header :: [(Text, Text)] -> Function -> Doc
          header ass f = "\n*"       <+>
                         pretty (funName f) <+>
                         arguments ass    <+>
                         "="         <+>
                         result ass

          arguments :: [(Text, Text)] -> Doc
          arguments ass =
                let vals = map snd
                         $ sortOn fst
                         $ mapMaybe (\(t, v) -> (,v) <$> getArgIndex t)
                           ass
                in hsep (map pretty vals)

          result :: [(Text, Text)] -> Doc
          result = pretty . fromMaybe "no result!" . lookup "$result"

          body :: [(Text, Text)] -> Doc
          body svs = vsep $ map bodyLine svs

          bodyLine :: (Text, Text) -> Doc
          bodyLine (t, v) = pretty t <+> "=" <+> pretty v

          -- getArgIndex $arg19 = Just 19
          getArgIndex :: Text -> Maybe Int
          getArgIndex (T.stripPrefix "$arg" -> Just rest) = case T.decimal(T.takeWhile isDigit rest) of Left e -> Nothing ; Right(i,rest) -> Just i
          getArgIndex _ = Nothing

-- | Save information about observed functions to the specified file, in HTML format.
debugSaveTrace :: FilePath -> DebugTrace -> IO ()
debugSaveTrace file db = do
    html  <- TL.readFile =<< getDataFileName "html/debug.html"
    debug <- TL.readFile =<< getDataFileName "html/debug.js"
    css   <- TL.readFile =<< getDataFileName "html/debug.css"
    let trace = encodeToLazyText db
    let script a = "<script>\n" <> a <> "\n</script>"
    let style a = "<style>\n" <> a <> "\n</style>"
    let f x | "trace.js" `TL.isInfixOf` x = script ("var trace =\n" <> trace <> ";")
            | "debug.js" `TL.isInfixOf` x = script debug
            | "debug.css" `TL.isInfixOf` x = style css
            | otherwise = x
    TL.writeFile file $ TL.unlines $ map f $ TL.lines html

-- | Open a web browser showing information about observed functions.
debugViewTrace :: DebugTrace -> IO ()
debugViewTrace db = do
    tdir <- getTemporaryDirectory
    file <- bracket
        (openTempFile tdir "debug.html")
        (hClose . snd)
        (return . fst)
    debugSaveTrace file db
    b <- openBrowser file
    unless b $
        putStrLn $
            "Failed to start a web browser, open: " ++ file ++ "\n" ++
            "In future you may wish to use 'debugSaveTrace."

#if __GLASGOW_HASKELL__ >= 800
-- On older GHC's this level of overlap leads to a compile error

-- | An orphan instance of 'Show' that maps anything without a 'Show' instance
--   to @?@. Suitable for use only when debugging.
instance {-# OVERLAPS #-} Show a where
    show _ = "?"
#endif

---------------------------------
-- Json output

-- | Obtain information about observed functions in JSON format.
--   The JSON format is not considered a stable part of the interface,
--   more presented as a back door to allow exploration of alternative
--   views.
debugJSONTrace :: DebugTrace -> B.ByteString
debugJSONTrace = encode

-- | A flat encoding of debugging observations.
data DebugTrace = DebugTrace
  { functions :: [Function]  -- ^ Flat list of all the functions traced
  , variables :: [Text]    -- ^ Flat list of all the variable values observed
  , calls     :: [CallData]  -- ^ Flat list of all the function calls traced
  }
  deriving (Eq, Generic, Show)

instance FromJSON DebugTrace
instance ToJSON DebugTrace where
  toEncoding = genericToEncoding defaultOptions
instance NFData DebugTrace

-- | A flat encoding of an observed call.
data CallData = CallData
  { callFunctionId :: Int       -- ^ An index into the 'functions' table
  , callVals :: [(Text, Int)] -- ^ The value name tupled with an index into the 'variables' table
  , callDepends :: [Int]        -- ^ Indexes into the 'calls' table
  , callParents :: [Int]        -- ^ Indexes into the 'calls' table
  }
  deriving (Eq, Generic, Show)

instance NFData CallData

instance FromJSON CallData where
  parseJSON (Object v) =
    CallData <$> v .: "" <*> vals <*> (fromMaybe [] <$> (v .:? "$depends")) <*>
    (fromMaybe [] <$> (v .:? "$parents"))
    where
      vals =
        sequence
          [ (k, ) <$> parseJSON x
          | (k, x) <- HM.toList v
          , not (T.null k)
          , k /= "$depends"
          , k /= "$parents"
          ]
  parseJSON invalid = typeMismatch "CallData" invalid

instance ToJSON CallData where
  toJSON CallData {..} =
    object $
    "" .= callFunctionId :
    ["$depends" .= toJSON callDepends | not (null callDepends)] ++
    ["$parents" .= toJSON callParents | not (null callParents)] ++
    map (uncurry (.=)) callVals
  toEncoding CallData {..} =
    pairs
      ("" .= callFunctionId <> depends <> parents <> foldMap (uncurry (.=)) callVals)
    where
      depends
        | null callDepends = mempty
        | otherwise = "$depends" .= callDepends
      parents
        | null callParents = mempty
        | otherwise = "$parents" .= callParents

functionJsonOptions :: Options
functionJsonOptions = defaultOptions{fieldLabelModifier = f}
    where
        f x | Just (x:xs) <- stripPrefix "fun" x = toLower x : xs
            | otherwise = x

instance FromJSON Function where
    parseJSON = genericParseJSON functionJsonOptions

instance ToJSON Function where
    toJSON = genericToJSON functionJsonOptions
    toEncoding = genericToEncoding functionJsonOptions
