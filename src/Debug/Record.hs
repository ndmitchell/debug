{-# LANGUAGE FlexibleInstances #-}
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
    -- * Viewing
    debugClear,
    debugPrint, debugJSON,
    debugView, debugSave
    ) where

import Debug.Variables
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List.Extra
import System.IO
import System.Directory
import System.IO.Unsafe
import Text.Show.Functions() -- Make sure the Show for functions instance exists
import qualified Data.Map as Map
import qualified Language.Javascript.JQuery as JQuery
import Web.Browser
import Paths_debug


-- | Metadata about a function, used to drive the HTML view.
data Function = Function
    {funName :: String -- ^ Function name
    ,funSource :: String -- ^ Function source, using @\n@ to break lines
    ,funArguments :: [String] -- ^ Variables for the arguments to the function
    ,funResult :: String -- ^ Variable for the result of the function
    }
    deriving (Eq,Ord,Show)

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

-- | Print information about the observed function calls to 'stdout'.
--   Definitely not machine readable, usually not human readable either.
debugPrint :: IO ()
debugPrint = do
    funs <- readIORef refCalls
    forM_ (reverse funs) $ \(Call name vars) -> do
        putStrLn $ funName name
        vars <- readIORef vars
        forM_ (reverse vars) $ \(name, v) ->
            putStrLn $ "  " ++ name ++ " = " ++ show v

-- | Obtain information about observed functions in JSON format.
--   The JSON format is not considered a stable part of the interface,
--   more presented as a back door to allow exploration of alternative
--   views.
debugJSON :: IO String
debugJSON = do
    vars <- readIORef refVariables
    vars <- return $ map (jsonString . varShow) $ listVariables vars
    calls <- readIORef refCalls
    let infos = nubOrd [x | Call x _ <- calls]
    let infoId = Map.fromList $ zip infos [0::Int ..]
    let funs = [jsonMap
            [("name",show funName)
            ,("source",show funSource)
            ,("arguments",show funArguments)
            ,("result",show funResult)
            ]
            | Function{..} <- infos]
    calls <- forM (reverse calls) $ \(Call info vars) -> do
        vars <- readIORef vars
        return $ jsonMap $ ("", show $ infoId Map.! info) : [(k, show $ varId v) | (k, v) <- reverse vars]
    return $
        "{\"functions\":\n" ++ jsonList funs ++
        ",\"variables\":\n" ++ jsonList vars ++
        ",\"calls\":\n" ++ jsonList (nubOrd calls) ++
        "}"
    where
        jsonList [] = "  []"
        jsonList (x:xs) = unlines $ ("  [" ++ x) : map ("  ," ++) xs ++ ["  ]"]
        jsonMap xs = "{" ++ intercalate "," [jsonString k ++ ":" ++ v | (k,v) <- xs] ++ "}"
        jsonString = show

-- | Save information about observed functions to the specified file, in HTML format.
debugSave :: FilePath -> IO ()
debugSave file = do
    html <- readFile =<< getDataFileName "html/debug.html"
    debug <- readFile =<< getDataFileName "html/debug.js"
    jquery <- readFile =<< JQuery.file
    trace <- debugJSON
    let script a = "<script>\n" ++ a ++ "\n</script>"
    let f x | "trace.js" `isInfixOf` x = script ("var trace =\n" ++ trace ++ ";")
            | "debug.js" `isInfixOf` x = script debug
            | "code.jquery.com/jquery" `isInfixOf` x = script jquery
            | otherwise = x
    writeFile file $ unlines $ map f $ lines html

-- | Open a web browser showing information about observed functions.
debugView :: IO ()
debugView = do
    tdir <- getTemporaryDirectory
    file <- bracket
        (openTempFile tdir "debug.html")
        (hClose . snd)
        (return . fst)
    debugSave file
    b <- openBrowser file
    unless b $ do
        putStrLn $
            "Failed to start a web browser, open: " ++ file ++ "\n" ++
            "In future you may wish to use 'debugSave'."


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
