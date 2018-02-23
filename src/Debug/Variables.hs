{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
{-# OPTIONS -fno-warn-unused-matches #-}
{-# OPTIONS -fno-warn-unused-imports #-}
    -- Any moved from GHC.Prim to GHC.Types
    -- so import both and use unused imports to get compatibility

-- | Module for debugging Haskell programs. To use, take the functions that
--   you are interested in debugging, e.g.:
--
-- > module QuickSort(quicksort) where
-- > import Data.List
-- >
-- > quicksort :: Ord a => [a] -> [a]
-- > quicksort [] = []
-- > quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
-- >     where (lt, gt) = partition (<= x) xs
--
--   Turn on the @TemplateHaskell@ and @ViewPatterns@ extensions, import "Debug",
--   indent your code and place it under a call to 'debug', e.g.:
--
-- > {-# LANGUAGE TemplateHaskell, ViewPatterns, PartialTypeSignatures #-}
-- > {-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- > module QuickSort(quicksort) where
-- > import Data.List
-- > import Debug
-- >
-- > debug [d|
-- >    quicksort :: Ord a => [a] -> [a]
-- >    quicksort [] = []
-- >    quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
-- >        where (lt, gt) = partition (<= x) xs
-- >    |]
--
--   We can now run our debugger with:
--
-- > $ ghci QuickSort.hs
-- > GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
-- > [1 of 1] Compiling QuickSort        ( QuickSort.hs, interpreted )
-- > Ok, 1 module loaded.
-- > *QuickSort> quicksort "haskell"
-- > "aehklls"
-- > *QuickSort> debugView
--
--   The final call to 'debugView' starts a web browser to view the recorded information.
--   Alternatively call 'debugSave' to write the web page to a known location.
--
--   For more ways to view the result (e.g. producing JSON) or record traces (without using
--   @TemplateHaskell@) see "Debug.DebugTrace".
module Debug.Variables(
    debug,
    debugClear,
    debugRun,
    debugPrint,
    debugJSON,
    debugView,
    debugSave,
    DebugTrace(..),
    getDebugTrace,
    -- * Recording
    funInfo, fun, var,
    ) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Extra
import           Data.Aeson
import           Data.Aeson.Text
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import Data.Generics.Uniplate.Data
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import           Data.IORef
import           Data.List.Extra
import           Data.Maybe
import           Data.Monoid                ()
import           Data.Text                  (Text, pack)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.IO          as TL
import           Data.Text.Read             as T
import           Data.Tuple.Extra
import qualified Data.Vector                as V
import           Debug.DebugTrace
import           Debug.Util
import           GHC.Generics
import           GHC.Prim
import           GHC.Types
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.Directory
import           System.IO
import           System.IO.Unsafe
import           Text.Show.Functions        ()
import           Unsafe.Coerce

-- | Run a computation and open a browser window showing observed function calls.
--
--   @ main = debugRun $ do
--       ...
--   @
debugRun :: IO a -> IO a
debugRun = bracket_ debugClear debugView

-- | Print information about the observed function calls to 'stdout',
--   in a human-readable format.
debugPrint :: IO ()
debugPrint = getDebugTrace >>= debugPrintTrace

-- | Save information about observed functions to the specified file, in HTML format.
debugSave :: FilePath -> IO ()
debugSave fp = debugSaveTrace fp =<< getDebugTrace

-- | Open a web browser showing information about observed functions.
debugView :: IO ()
debugView = getDebugTrace >>= debugViewTrace

-- | Obtain information about observed functions in JSON format.
--   The JSON format is not considered a stable part of the interface,
--   more presented as a back door to allow exploration of alternative
--   views.
debugJSON :: IO String
debugJSON = B.unpack . debugJSONTrace <$> getDebugTrace

------------------------------------------------------------------
-- DebugTrace Backend

-- | A single function call, used to attach additional information
data Call = Call Function (IORef [(Text, Var)])

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

-- | Returns all the information about the observed function accumulated so far
--   in the variables.
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
          callDepends = [] -- available in the Hoed backend but not in this one
          callParents = [] -- available in the Hoed backend but not in this one
      return CallData{..}
  return $ DebugTrace infos (map T.pack vars) callEntries

------------------------------------------------------------------
-- Instrumentation

{-# NOINLINE fun #-}
-- | Called under a lambda with a function name to provide a unique context for
--   a particular call, e.g.:
--
-- > tracedAdd x y = fun "add" $ \t -> var t "x" x + var t "y" y
--
--   This function involves giving identity to function calls, so is unsafe,
--   and will only work under a lambda.
fun :: Show a => String -> (Call -> a) -> a
fun name = funInfo $ Function (T.pack name) "" [] ""

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
    when (show val /= "<function>") $ do -- these make the variable list long without providing useful info
        var <- atomicModifyIORef refVariables $ addVariable val
        name' <- unShadowName ref $ pack name
        whenJust name' (\n -> atomicModifyIORef ref $ \v -> ((n, var) :v, ()))
    return val

-- | If a name is already used, find the next available name by adding ' (prime) chars until
--   the resulting name is unique
unShadowName :: IORef [(Text, Var)] -> Text -> IO (Maybe Text)
unShadowName ioRef t = do
    pairs <- readIORef ioRef
    let matches = filter (isPrefixPrime t) $ map fst pairs
    let shadowedLimit = 3
    if not (null matches)
        then do
            let lengths = map T.length matches
            let zipped = zip matches lengths
            let maxLen = maximum lengths
            let maxName = fst $ fromJust $ find (\p -> snd p == maxLen) zipped
            case length matches of    -- --i.e., case (the number of (')s)
                n | n > shadowedLimit  -> return Nothing
                _                      -> return $ Just $ maxName `T.append` "'"
        else return $ Just t

-- | Is the second string equal to the first plus some number of ' (prime) characters?
-- | e.g. x `isPrefixPrime` x' == true, x isPrefixPrime x'' == True, but x isPrefixPrime xs == False
isPrefixPrime :: Text -> Text -> Bool
isPrefixPrime s t = s == T.dropWhileEnd (== '\'') t

-- | A @TemplateHaskell@ wrapper to convert a normal function into a traced function.
--   For an example see "Debug". Inserts 'funInfo' and 'var' calls.
debug :: Q [Dec] -> Q [Dec]
debug q = do
    missing <- filterM (notM . isExtEnabled) [ViewPatterns, PartialTypeSignatures]
    when (missing /= []) $
        error $ "\ndebug [d| ... |] requires additional extensions:\n" ++
                "{-# LANGUAGE " ++ intercalate ", " (map show missing) ++ " #-}\n"
    decs <- q
    let askSig x = find (\case SigD y _ -> x == y; _ -> False) decs
    mapM (adjustDec askSig) decs

adjustDec :: (Name -> Maybe Dec) -> Dec -> Q Dec
-- try and shove in a "_ =>" if we can, to capture necessary Show instances
adjustDec askSig x@(SigD name ty@(ForallT vars ctxt typ))
  | hasRankNTypes ty = return x
  | otherwise = return $
    SigD name $ ForallT vars (delete WildCardT ctxt ++ [WildCardT]) typ
adjustDec askSig (SigD name typ) = adjustDec askSig $ SigD name $ ForallT [] [] typ
adjustDec askSig o@(FunD name clauses@(Clause arity _ _:_))
  | Just (SigD _ ty) <- askSig name
  , hasRankNTypes ty = return o
  | otherwise = do
    inner <- newName "inner"
    tag <- newName "tag"
    args <- sequence [newName $ "arg" ++ show i | i <- [1 .. length arity]]
    let addTag (Clause ps bod inner) = Clause (VarP tag:ps) bod inner
    let clauses2 = map addTag $ transformBi (adjustPat tag) clauses
    let args2 = [VarE 'var `AppE` VarE tag `AppE` toLitPre "$" a `AppE` VarE a | a <- args]
    let info = ConE 'Function `AppE`
            packLit (toLit name) `AppE`
            packLit (LitE (StringL $ prettyPrint $ maybeToList (askSig name) ++ [o])) `AppE`
            ListE (map (packLit . toLitPre "$") args) `AppE`
            packLit (LitE (StringL "$result"))
    let body2 = VarE 'var `AppE` VarE tag `AppE` LitE (StringL "$result") `AppE` foldl AppE (VarE inner) (VarE tag : args2)
    let body = VarE 'funInfo `AppE` info `AppE` LamE [VarP tag] body2
    afterApps <- transformApps tag clauses2
    return $ FunD name [Clause (map VarP args) (NormalB body) [FunD inner afterApps]]
adjustDec askSig x = return x

transformApps :: Name -> [Clause] -> Q [Clause]
transformApps tag = mapM (appsFromClause tag)

appsFromClause :: Name -> Clause -> Q Clause
appsFromClause tag cl@(Clause pats body decs) = do
    newBody <- appsFromBody tag body
    newDecs <- mapM (appsFromDec tag) decs
    return $ Clause pats newBody newDecs

appsFromBody :: Name -> Body -> Q Body
appsFromBody _ b@(GuardedB _) = return b -- TODO: implement guards
appsFromBody tag (NormalB e)  = NormalB <$> appsFromExp tag e

appsFromExp :: Name -> Exp -> Q Exp
appsFromExp tag e@(AppE e1 e2) = do
    newE1 <- appsFromExp tag e1
    newE2 <- appsFromExp tag e2
    adjustApp tag (AppE newE1 newE2)
appsFromExp tag e@(LetE decs exp) = do
    newDecs <- traverse (appsFromDec tag) decs
    LetE newDecs <$> appsFromExp tag exp
appsFromExp tag e@(InfixE e1May e2 e3May) = do
    newE1 <- appsFromExpMay tag e1May
    newE2 <- appsFromExp tag e2
    newE3 <- appsFromExpMay tag e3May
    adjustApp tag (InfixE newE1 newE2 newE3)
appsFromExp tag e@(CaseE exp matches) = do
    newExp <- appsFromExp tag exp
    newMatches <- traverse (appsFromMatch tag) matches
    return $ CaseE newExp newMatches
appsFromExp tag e = return e

appsFromExpMay :: Name -> Maybe Exp -> Q (Maybe Exp)
appsFromExpMay tag Nothing  = return Nothing
appsFromExpMay tag (Just e) = sequence $ Just $ appsFromExp tag e

appsFromDec :: Name -> Dec -> Q Dec
appsFromDec tag d@(ValD pat body dec) = do
    newBody <- appsFromBody tag body
    return $ ValD pat newBody dec
appsFromDec tag d@(FunD name subClauses) =
    FunD name <$> traverse (appsFromClause tag) subClauses
appsFromDec _ d = return d

appsFromMatch :: Name -> Match -> Q Match
appsFromMatch tag (Match pat body decs) = do
    newBody <- appsFromBody tag body
    newDecs <- traverse (appsFromDec tag) decs
    return $ Match pat newBody newDecs

adjustApp :: Name -> Exp -> Q Exp
adjustApp tag (AppE e1 e2) = do
    let displayName = expDisplayName e1
    e1n <- newName displayName
    let viewP = ViewP (VarE 'var `AppE` VarE tag `AppE` LitE (StringL displayName)) (VarP e1n)
    let result = LetE [ValD viewP (NormalB (AppE e1 e2)) []] (VarE e1n)
    return result
adjustApp tag e@(InfixE e1May e2 e3May) = do
    let displayName = infixExpDisplayName e2 -- infix symbol, e.g "++"
    if displayName == "$" --don't record $ as a function application
        then return e
        else do
            let legalInfixVar = mkLegalInfixVar displayName -- infix name as valid variable, e.g. "plus_plus"
            e2Var <- newName legalInfixVar
            let viewP = ViewP (VarE 'var `AppE` VarE tag `AppE` LitE (StringL displayName)) (VarP e2Var)
            return $ LetE [ValD viewP (NormalB (InfixE e1May e2 e3May)) []] (VarE e2Var)
adjustApp _ e@(UInfixE e1 e2 e3) = return e   --TODO: These might need to be processed
adjustApp _ e = return e

-- Find the (unqualified) function name to use as the UI display name
expDisplayName :: Exp -> String
expDisplayName e =
    let name = removeLet $ (show . ppr) e
    in removeExtraDigits (takeWhileEnd (/= '.') ((head . words) name))

-- Same as expDisplayName but for infix functions
infixExpDisplayName :: Exp -> String
infixExpDisplayName e =
    let name = removeLet $ (show . ppr) e
        name' = removeExtraDigits (takeWhileEnd (/= '.') ((head . words) name))
    in fromMaybe name' $ stripSuffix ")" name'

adjustPat :: Name -> Pat -> Pat
adjustPat tag (VarP x) = ViewP (VarE 'var `AppE` VarE tag `AppE` toLit x) (VarP x)
adjustPat tag x = x

toLit :: Name -> Exp
toLit = toLitPre ""
toLitPre :: String -> Name -> Exp
toLitPre pre (Name (OccName x) _) = LitE $ StringL $ pre ++ x
packLit :: Exp -> Exp
packLit = AppE (VarE 'pack)

------------------------------------------------------------------
-- Data structures

data Variables = Variables
    Int -- Number in the array
    [(Any, String)] -- Entries, (key a, show a), indexed from [n..0]

data Var = Var Int String -- index into Variables, show a
         deriving (Eq,Ord)

instance Show Var where
    show (Var i s) = s ++ " @" ++ show i

varId :: Var -> Int
varId (Var x _) = x

varShow :: Var -> String
varShow (Var _ x) = x

newVariables :: Variables
newVariables = Variables 0 []

listVariables :: Variables -> [Var]
listVariables (Variables n xs) = [Var i s | (i,(_,s)) <- zipFrom 0 $ reverse xs]

addVariable :: Show a => a -> Variables -> (Variables, Var)
addVariable a vs@(Variables n xs) =
    case findIndex (\(key,_) -> ptrEqual key keyA) xs of
        Nothing -> (Variables (n+1) ((keyA,showA):xs), Var n showA)
        Just i  -> (vs, Var (n-i-1) $ snd $ xs !! i)
    where
        keyA = unsafeCoerce a
        showA = show a

ptrEqual :: Any -> Any -> Bool
ptrEqual a b = unsafePerformIO $ do
    a <- evaluate a
    b <- evaluate b
    return $ isTrue# (reallyUnsafePtrEquality# a b)
