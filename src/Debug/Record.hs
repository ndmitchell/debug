{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Dodgy Show instance, useful for debugging

-- | Module for recording and manipulating debug traces. For most users, the
--   @TemplateHaskell@ helpers in "Debug" should be sufficient.
module Debug.Record(
    -- * Recording
    Function(..),
    Call,
    funInfo, fun, var,
    debugClear,
    -- * Viewing
    debugPrint, debugPrintTrace,
    debugJSON, debugJSONTrace,
    debugView, debugViewTrace,
    debugSave, debugSaveTrace,
    -- * Exporting
    getDebugTrace,
    DebugTrace(..),
    CallData(..)
    ) where

import Debug.Variables
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Text
import Data.Aeson.Types
import Data.Char
import Data.Hashable
import Data.IORef
import Data.List.Extra
import Data.Maybe
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
import System.IO.Unsafe
import Text.Show.Functions() -- Make sure the Show for functions instance exists
import qualified Language.Javascript.JQuery as JQuery
import Web.Browser
import Paths_debug
import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))
import Text.Read


-- | Metadata about a function, used to drive the HTML view.
data Function = Function
    {funName :: String -- ^ Function name
    ,funSource :: String -- ^ Function source, using @\n@ to break lines
    ,funArguments :: [String] -- ^ Variables for the arguments to the function
    ,funResult :: String -- ^ Variable for the result of the function
    }
    deriving (Eq,Generic,Ord,Show)

instance Hashable Function

-- | A single function call, used to attach additional information
data Call = Call Function (IORef [(String, Var)])

{-# NOINLINE refVariables #-}
refVariables :: IORef Variables
refVariables = unsafePerformIO $ newIORef newVariables

{-# NOINLINE refCalls #-}
refCalls :: IORef [Call]
refCalls = unsafePerformIO $ newIORef []

-- | Clear all debug information. Useful when working in @ghci@ to reset
--   any previous debugging work and reduce the amount of output.
debugClear :: IO ()
debugClear = do
    writeIORef refVariables newVariables
    writeIORef refCalls []

-- | Print information about the observed function calls to 'stdout',
--   in a human-readable format.
debugPrint :: IO ()
debugPrint = getDebugTrace >>= debugPrintTrace

-- | Print information about the observed function calls to 'stdout',
--   in a human-readable format.
debugPrintTrace :: DebugTrace -> IO ()
debugPrintTrace DebugTrace{..} = do
    let lookupFun = (V.fromList functions V.!)
        lookupVar = (V.fromList variables V.!)
        concs = [(lookupFun callFunctionId, map (second lookupVar) callVals)
                | CallData{..} <- calls]
        docs = map call $ nubOrd $ reverse concs
    putDoc (vcat docs <> hardline)
    where
          call :: (Function, [(String, String)]) -> Doc
          call (f, vs) =
                   let ass = vs
                       hdr = bold $ header ass f
                   in hang 5 $ hdr <$$> body ass


          header :: [(String, String)] -> Function -> Doc
          header ass f = text "\n*"       <+>
                         text (funName f) <+>
                         arguments ass    <+>
                         text "="         <+>
                         result ass

          arguments :: [(String, String)] -> Doc
          arguments ass =
                let vals = map snd
                         $ sortOn fst
                         $ mapMaybe (\(t, v) -> (,v) <$> getArgIndex t)
                           ass
                in hsep (map text vals)

          result :: [(String, String)] -> Doc
          result = text . fromMaybe "no result!" . lookup "$result"

          body :: [(String, String)] -> Doc
          body svs = vsep $ map bodyLine svs

          bodyLine :: (String, String) -> Doc
          bodyLine (t, v) = text t <+> text "=" <+> text v

          -- getArgIndex $arg19 = Just 19
          getArgIndex :: String -> Maybe Int
          getArgIndex ('$':'a':'r':'g':rest) = readMaybe(takeWhile isDigit rest)
          getArgIndex _ = Nothing

-- | Save information about observed functions to the specified file, in HTML format.
debugSave :: FilePath -> IO ()
debugSave fp = debugSaveTrace fp =<< getDebugTrace

-- | Save information about observed functions to the specified file, in HTML format.
debugSaveTrace :: FilePath -> DebugTrace -> IO ()
debugSaveTrace file db = do
    html <- TL.readFile =<< getDataFileName "html/debug.html"
    debug <- TL.readFile =<< getDataFileName "html/debug.js"
    jquery <- TL.readFile =<< JQuery.file
    trace <- encodeToLazyText <$> getDebugTrace
    let script a = "<script>\n" <> a <> "\n</script>"
    let f x | "trace.js" `TL.isInfixOf` x = script ("var trace =\n" <> trace <> ";")
            | "debug.js" `TL.isInfixOf` x = script debug
            | "code.jquery.com/jquery" `TL.isInfixOf` x = script jquery
            | otherwise = x
    TL.writeFile file $ TL.unlines $ map f $ TL.lines html

-- | Open a web browser showing information about observed functions.
debugView :: IO ()
debugView = getDebugTrace >>= debugViewTrace

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

{-# NOINLINE fun #-}
-- | Called under a lambda with a function name to provide a unique context for
--   a particular call, e.g.:
--
-- > tracedAdd x y = fun "add" $ \t -> var t "x" x + var t "y" y
--
--   This function involves giving identity to function calls, so is unsafe,
--   and will only work under a lambda.
fun :: Show a => String -> (Call -> a) -> a
fun name = funInfo $ Function name [] [] []

-- | A version of 'fun' allowing you to pass further information about the
--   'Function' which is used when showing debug views.
funInfo :: Show a => Function -> (Call -> a) -> a
{-# NOINLINE funInfo #-}
funInfo info f = unsafePerformIO $ do
    ref <- newIORef []
    let x = Call info ref
    atomicModifyIORef refCalls $ \v -> (x:v, ())
    return $ f x

{-# NOINLINE var #-}
-- | Used in conjunction with 'fun' to annotate variables. See 'fun' for an example.
var :: Show a => Call -> String -> a -> a
var (Call _ ref) name val = unsafePerformIO $ do
    var <- atomicModifyIORef refVariables $ addVariable val
    atomicModifyIORef ref $ \v -> ((name, var):v, ())
    return val

---------------------------------
-- Json output

-- | Obtain information about observed functions in JSON format.
--   The JSON format is not considered a stable part of the interface,
--   more presented as a back door to allow exploration of alternative
--   views.
debugJSON :: IO String
debugJSON = B.unpack . debugJSONTrace <$> getDebugTrace

-- | Obtain information about observed functions in JSON format.
--   The JSON format is not considered a stable part of the interface,
--   more presented as a back door to allow exploration of alternative
--   views.
debugJSONTrace :: DebugTrace -> B.ByteString
debugJSONTrace = encode

-- | A flat encoding of debugging observations.
data DebugTrace = DebugTrace
  { functions :: [Function]  -- ^ Flat list of all the functions traced
  , variables :: [String]    -- ^ Flat list of all the variable values observed
  , calls     :: [CallData]  -- ^ Flat list of all the function calls traced
  }
  deriving (Eq, Generic, Show)

-- | Returns all the information about the observed function accumulated so far.
getDebugTrace :: IO DebugTrace
getDebugTrace = do
  vars <- readIORef refVariables
  vars <- return $ map varShow $ listVariables vars
  calls <- readIORef refCalls
  let infos = nubOrd [x | Call x _ <- calls]
      infoId = HM.fromList $ zip infos [0::Int ..]
  callEntries <-
    forM (reverse calls) $ \(Call info vars) -> do
      vars <- readIORef vars
      let callFunctionId   = infoId HM.! info
          callVals = map (second varId) vars
      return CallData{..}
  return $ DebugTrace infos vars callEntries

instance FromJSON DebugTrace
instance ToJSON DebugTrace

-- | A flat encoding of an observed call.
data CallData = CallData
  { callFunctionId :: Int       -- ^ An index into the 'functions' table
  , callVals :: [(String, Int)] -- ^ The value name tupled with an index into the 'variables' table
  }
  deriving (Eq, Generic, Show)

instance FromJSON CallData where
  parseJSON (Object v) = CallData <$> v .: "" <*> vals
    where
      vals = mapM (\(k,x) -> (T.unpack k,) <$> parseJSON x) $ HM.toList v
  parseJSON invalid = typeMismatch "CallData" invalid

instance ToJSON CallData where
  toJSON CallData {..} =
    object $
    "" .= callFunctionId : map (uncurry (.=) . first T.pack) callVals

functionJsonOptions =
  defaultOptions {fieldLabelModifier = functionLabelModifier}
  where
    functionLabelModifier "funName" = "name"
    functionLabelModifier "funSource" = "source"
    functionLabelModifier "funArguments" = "arguments"
    functionLabelModifier "funResult" = "result"

instance FromJSON Function where
  parseJSON = genericParseJSON functionJsonOptions

instance ToJSON Function where
  toJSON = genericToJSON functionJsonOptions
