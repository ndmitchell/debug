{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Dodgy Show instance, useful for debugging

module Debug.Record(
    debugClear,
    debugConsole, debugJS, debugHTML,
    Function(..),
    Call,
    funInfo, fun, var
    ) where

import Debug.Variables
import Control.Monad
import Data.IORef
import Data.List.Extra
import System.IO.Unsafe
import Text.Show.Functions() -- Make sure the Show for functions instance exists
import qualified Data.Map as Map
import qualified Language.Javascript.JQuery as JQuery
import Paths_debug



data Function = Function
    {funName :: String
    ,funSource :: String
    ,funArguments :: [String]
    ,funResult :: String
    }
    deriving (Eq,Ord,Show)

data Call = Call Function (IORef [(String, Var)])

{-# NOINLINE refVariables #-}
refVariables :: IORef Variables
refVariables = unsafePerformIO $ newIORef newVariables

{-# NOINLINE refCalls #-}
refCalls :: IORef [Call]
refCalls = unsafePerformIO $ newIORef []

debugClear :: IO ()
debugClear = do
    writeIORef refVariables newVariables
    writeIORef refCalls []

debugConsole :: IO ()
debugConsole = do
    funs <- readIORef refCalls
    forM_ (reverse funs) $ \(Call name vars) -> do
        putStrLn $ funName name
        vars <- readIORef vars
        forM_ (reverse vars) $ \(name, v) ->
            putStrLn $ "  " ++ name ++ " = " ++ show v

debugJS :: IO String
debugJS = do
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
        "var traceFunctions =\n" ++ jsonList funs ++
        "var traceVariables =\n" ++ jsonList vars ++
        "var traceCalls =\n" ++ jsonList (nubOrd calls)
    where
        jsonList [] = "  []"
        jsonList (x:xs) = unlines $ ("  [" ++ x) : map ("  ," ++) xs ++ ["  ]"]
        jsonMap xs = "{" ++ intercalate "," [jsonString k ++ ":" ++ v | (k,v) <- xs] ++ "}"
        jsonString = show

debugHTML :: IO String
debugHTML = do
    html <- readFile =<< getDataFileName "html/debug.html"
    debug <- readFile =<< getDataFileName "html/debug.js"
    jquery <- readFile =<< JQuery.file
    trace <- debugJS
    let script a = "<script>\n" ++ a ++ "\n</script>"
    let f x | "trace.js" `isInfixOf` x = script trace
            | "debug.js" `isInfixOf` x = script debug
            | "code.jquery.com/jquery" `isInfixOf` x = script jquery
            | otherwise = x
    return $ unlines $ map f $ lines html

#if __GLASGOW_HASKELL__ >= 800
-- On older GHC's this level of overlap leads to a compile error
instance {-# OVERLAPS #-} Show a where
    show _ = "?"
#endif

{-# NOINLINE fun #-}
fun :: Show a => String -> (Call -> a) -> a
fun name = funInfo $ Function name [] [] []

{-# NOINLINE funInfo #-}
funInfo :: Show a => Function -> (Call -> a) -> a
funInfo info f = unsafePerformIO $ do
    ref <- newIORef []
    let x = Call info ref
    atomicModifyIORef refCalls $ \v -> (x:v, ())
    return $ f x

{-# NOINLINE var #-}
var :: Show a => Call -> String -> a -> a
var (Call _ ref) name val = unsafePerformIO $ do
    var <- atomicModifyIORef refVariables $ addVariable val
    atomicModifyIORef ref $ \v -> ((name, var):v, ())
    return val
