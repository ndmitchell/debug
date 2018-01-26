{-# LANGUAGE CPP               #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS -Wno-orphans       #-}
-- | An alternative backend for lazy debugging with call stacks built on top of the "Hoed" package.
--
--   Instrumentation is done via a TH wrapper, which requires the following extensions:
--
--  - 'TemplateHaskell'
--  - 'PartialTypeSignatures'
--  - 'ViewPatterns'
--  - 'ExtendedDefaultRules'
--  - 'FlexibleContexts'
--
--   Moreover, 'Observable' instances are needed for value inspection. The 'debug'' template haskell wrapper can automatically insert these for 'Generic' types.
--
-- > {-# LANGUAGE TemplateHaskell, ViewPatterns, PartialTypeSignatures, ExtendedDefaultRules #-}
-- > {-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- > module QuickSort(quicksort) where
-- > import Data.List
-- > import Debug.Hoed
-- >
-- > debug [d|
-- >    quicksort :: Ord a => [a] -> [a]
-- >    quicksort [] = []
-- >    quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
-- >        where (lt, gt) = partition (<= x) xs
-- >    |]
--
-- Now we can debug our expression under 'debugRun':
--
-- > $ ghci examples/QuickSortHoed.hs
-- > GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
-- > [1 of 1] Compiling QuickSortHoed    ( QuickSortHoed.hs, interpreted )
-- > Ok, 1 module loaded.
-- > *QuickSort> debugRun $ quicksort "haskell"
-- > "aehklls"
--
-- After our expression is evaluated a web browser is started displaying the recorded
-- information.
--
-- To debug an entire program, wrap the 'main' function below 'debugRun'.
module Debug.Hoed
  (
    debug
  , debug'
  , Config(..)
  , debugRun
    -- * Generate a trace
  , getDebugTrace
    -- * Reexported from Hoed
  , Observable(..)
  , observe
  , HoedOptions(..)
  , defaultHoedOptions
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Data
import           Data.Generics.Uniplate.Data
import           Data.Graph.Libgraph
import           Data.Hashable
import qualified Data.HashMap.Monoidal       as HM
import qualified Data.HashMap.Strict         as HMS
import qualified Data.Map.Strict             as Map
import qualified Data.HashSet                as Set
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as T
import "Hoed"    Debug.Hoed
import           Debug.Hoed.Render
import           Debug.Record                as D (CallData (..),
                                                   DebugTrace (..),
                                                   Function (..),
                                                   debugViewTrace
                                                   )
import           GHC.Exts                    (IsList (..))
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.Clock

{-# ANN module ("hlint: ignore Redundant bracket" :: String) #-}

-- | Runs the program collecting a debugging trace and then opens a web browser to inspect it.
--
--   @ main = debugRun $ do
--       ...
--   @
debugRun :: IO () -> IO ()
debugRun program = getDebugTrace defaultHoedOptions {prettyWidth = 160, verbose = Verbose} program >>= debugViewTrace

-- | Runs the program collecting a debugging trace
getDebugTrace :: HoedOptions -> IO () -> IO DebugTrace
getDebugTrace hoedOptions program = do
  hoedAnalysis <- runO' hoedOptions program
  putStrLn "Please wait while the debug trace is constructed..."
  let !compTree = hoedCompTree hoedAnalysis
  t <- getTime Monotonic
  let result = convert compTree
      !_     = length(variables result)
  t' <- getTime Monotonic
  let compTime = fromIntegral(toNanoSecs(diffTimeSpec t t')) * 1e-9
  putStrLn $ "=== Debug Trace (" ++ show compTime ++ " seconds) ==="
  return result

type a :-> b = HM.MonoidalHashMap a b

data HoedFunctionKey = HoedFunctionKey
  { label   :: !Text
  , arity   :: !Int
  , clauses :: ![Text]
  }
  deriving (Eq)

instance Hashable HoedFunctionKey where
  hashWithSalt s HoedFunctionKey{..} =
    s `hashWithSalt` label
      `hashWithSalt` arity
      `hashWithSalt` clauses

type HoedCallKey = Int

data HoedCallDetails = HoedCallDetails
  { argValues
  , clauseValues :: ![Hashed Text]
  , result :: !(Hashed Text)
  , depends, parents :: ![HoedCallKey]
  } deriving (Eq, Generic, Hashable)

-- XXX Remove these orphan instances when a version of Hoed with them is released
instance Hashable Vertex where
  hashWithSalt s RootVertex    = s `hashWithSalt` (-1 :: Int)
  hashWithSalt s (Vertex cs _) = s `hashWithSalt` cs
instance Hashable CompStmt where
  hashWithSalt s cs = hashWithSalt s (stmtIdentifier cs)

---------------------------------------------------------------------------
-- Cached pred and succ relationships

data AnnotatedCompTree = AnnotatedCompTree
  { compTree           :: CompTree
  , predsMap, succsMap:: HMS.HashMap Vertex [Vertex]
  }
getPreds :: AnnotatedCompTree -> Vertex -> [Vertex]
getPreds act v = fromMaybe [] $ HMS.lookup v (predsMap act)

getSuccs :: AnnotatedCompTree -> Vertex -> [Vertex]
getSuccs act v =  fromMaybe [] $ HMS.lookup v (succsMap act)

annotateCompTree :: CompTree -> AnnotatedCompTree
annotateCompTree compTree = AnnotatedCompTree{..}  where
  predsMap  = HMS.fromListWith (++) [ (t, [s]) | Arc s t _ <- arcs compTree]
  succsMap  = HMS.fromListWith (++) [ (s, [t]) | Arc s t _ <- arcs compTree]

---------------------------------------------------------------------------
hoedCallValues :: HoedCallDetails -> [Hashed Text]
hoedCallValues HoedCallDetails{..} = result : (argValues ++ clauseValues)

getRelatives rel v =
      [ stmtIdentifier
        | v'@Vertex {vertexStmt = CompStmt {stmtIdentifier, stmtDetails = StmtLam {}}} <- rel v
      ] ++
      [ callKey
        | v'@Vertex {vertexStmt = CompStmt {stmtDetails = StmtCon {}}} <- rel v
        , callKey <- getRelatives rel v'
      ]

extractHoedCall :: AnnotatedCompTree -> Vertex -> Maybe (Hashed HoedFunctionKey, HoedCallKey, HoedCallDetails)
extractHoedCall hoedCompTree v@Vertex {vertexStmt = c@CompStmt {stmtDetails = StmtLam {..}, ..}} =
  Just
    ( hashed $ HoedFunctionKey (pack stmtLabel) (length stmtLamArgs) (map fst clauses)
    , stmtIdentifier
    , HoedCallDetails (map (hashed . pack) stmtLamArgs) (map snd clauses) (hashed (pack stmtLamRes)) depends parents)
  where
    clauses =
      [ (pack stmtLabel, hashed (pack stmtCon))
      | Vertex {vertexStmt = CompStmt {stmtLabel, stmtDetails = StmtCon {..}}} <-
          getSuccs hoedCompTree v
      ]
    depends = snub $ getRelatives (getSuccs hoedCompTree) v
    parents = snub $ getRelatives (getPreds hoedCompTree) v

extractHoedCall _ _ = Nothing

-- | Convert a 'Hoed' trace to a 'debug' trace
convert :: CompTree -> DebugTrace
convert hoedCompTree = DebugTrace {..}
  where
    hoedFunctionCalls :: Hashed HoedFunctionKey :-> [(HoedCallKey, HoedCallDetails)]
    hoedFunctionCalls =
      HM.fromList
        [ (fnKey, [(callKey, callDetails)])
        | Just (fnKey, callKey, callDetails) <-
            map (extractHoedCall (annotateCompTree hoedCompTree)) (vertices hoedCompTree)
        ]
    sortedFunctionCalls =
      sortOn (\(unhashed -> x, _) -> (label x, arity x)) $ toList hoedFunctionCalls

    variablesHashed :: [Hashed Text]
    variablesHashed =
      Set.toList $
      Set.fromList $
      foldMap (foldMap (hoedCallValues . snd)) hoedFunctionCalls

    variables = map unhashed variablesHashed

    lookupFunctionIndex =
      fromMaybe (error "bug in convert: lookupFunctionIndex") .
      (`HMS.lookup` HMS.fromList (zip (map fst sortedFunctionCalls) [0 ..]))

    lookupVariableIndex =
      fromMaybe (error "bug in convert: lookupVariableIndex") .
      (`HMS.lookup` HMS.fromList (zip variablesHashed [0 ..]))

    lookupCallIndex =
      fromMaybe (error "bug in convert: lookupCallIndex") .
      (`HMS.lookup` HMS.fromList (zip (map fst callsTable) [0 ..]))

    (functions, concat -> callsTable) =
      unzip
      [ (Function{..}
        ,[( callId, CallData {..})
         | (callId, HoedCallDetails {..}) <- toList calls
         , let callVals =
                 map (second lookupVariableIndex) $
                 ("$result", result) :
                 zipWith (\i v -> ("$arg" <> pack (show i), v)) [1 ..] argValues ++
                 zip clauses clauseValues
         , let callDepends = map lookupCallIndex depends
         , let callParents = map lookupCallIndex parents
         ])
      | (k@(unhashed -> HoedFunctionKey {..}), calls) <- sortedFunctionCalls
      , let callFunctionId = lookupFunctionIndex k
      , let funResult = "$result"
      , let funArguments = map (\i -> "$arg" <> pack(show i)) [1 .. arity] ++ clauses
      -- HACK Expects a multiline label with the function name in the first line, and the code afterwards
      , let (funName,funSource) = T.break (=='\n') label
      ]

    calls = map snd callsTable

snub :: Ord a => [a] -> [a]
snub = map head . group . sort

----------------------------------------------------------------------------
-- Template Haskell

data Config = Config
  { generateGenericInstances      :: Bool      -- ^ Insert @deriving stock Generic@ on type declarations that don't already derive 'Generic'. Requires @DeriveGeneric@ and @DerivingStrategies@.
  , generateObservableInstances   :: Bool      -- ^ Insert @deriving anyclass Observable@ on type declarations that don't already derive 'Observable'. Requires @DeriveAnyClass@ and @DerivingStrategies@.
  , excludeFromInstanceGeneration :: [String]  -- ^ Exclude types from instance generation by name (unqualified).
  }

-- | A @TemplateHaskell@ wrapper to convert normal functions into traced functions.
debug = debug' (Config False False [])

-- | A @TemplateHaskell@ wrapper to convert normal functions into traced functions
--   and optionally insert 'Observable' and 'Generic' instances.
debug' :: Config -> Q [Dec] -> Q [Dec]
debug' Config{..} q = do
  missing <-
    filterM
      (fmap not . isExtEnabled)
      ([ ViewPatterns
       , PartialTypeSignatures
       , ExtendedDefaultRules
       , FlexibleContexts
       ]
#if __GLASGOW_HASKELL__ >= 802
       ++
       [DeriveAnyClass | generateObservableInstances] ++
       [DerivingStrategies | generateObservableInstances] ++
       [DeriveGeneric | generateGenericInstances]
#endif
      )
  when (missing /= []) $
    error $
    "\ndebug [d| ... |] requires additional extensions:\n" ++
    "{-# LANGUAGE " ++ intercalate ", " (map show missing) ++ " #-}\n"
  decs <- q
  let askSig x =
        listToMaybe $
        mapMaybe
          (\case
             SigD y s
               | x == y -> Just s
             _ -> Nothing)
          decs
  let checkSig = maybe True (not . hasRankNTypes) . askSig
  let sourceNames =
        mapMaybe
          (\case
             FunD n _ -> Just n
             ValD (VarP n) _ _ -> Just n
             _ -> Nothing)
          decs
  names <-
    sequence [(n, ) <$> newName (mkDebugName (nameBase n)) | n <- sourceNames]
  let excludedSet = Set.fromList excludeFromInstanceGeneration
        -- HACK We embed the source code of the function in the label,
        --      which is then unpacked by 'convert'
      createLabel n dec = nameBase n ++ "\n" ++ prettyPrint dec

#if __GLASGOW_HASKELL__ >= 820
      updateDerivs derivs
        | hasGenericInstance <- not $ null $ filterDerivingClausesByName ''Generic derivs
        = [ DerivClause (Just StockStrategy)    [ConT ''Generic]
          | not hasGenericInstance
          , generateGenericInstances
          ] ++
          [ DerivClause (Just AnyclassStrategy) [ConT ''Observable]
          | [] == filterDerivingClausesByName ''Observable derivs
          , hasGenericInstance || generateGenericInstances
          ] ++
          derivs
      filterDerivingClausesByName n' derivs =
        [ it | it@(DerivClause _ preds) <- derivs , ConT n <- preds , n == n']
#endif
  fmap concat $
    forM decs $ \dec ->
      case dec of
        ValD (VarP n) b clauses
          | checkSig n -> do
            let Just n' = lookup n names
                label = createLabel n dec
            newDecl <-
              funD n [clause [] (normalB [|observe label $(varE n')|]) []]
            let clauses' = transformBi adjustValD clauses
            return [newDecl, ValD (VarP n') b clauses']
        FunD n clauses
          | checkSig n -> do
            let Just n' = lookup n names
                label = createLabel n dec
            newDecl <-
              funD n [clause [] (normalB [|observe label $(varE n')|]) []]
            let clauses' = transformBi (adjustInnerSigD . adjustValD) clauses
            return [newDecl, FunD n' clauses']
        SigD n ty
          | Just n' <- lookup n names
          , not (hasRankNTypes ty) -> do
            let ty' = adjustTy ty
            ty'' <- renameForallTyVars ty'
            return [SigD n ty', SigD n' ty'']
#if __GLASGOW_HASKELL__ >= 820
        DataD cxt1 name tt k cons derivs
          | not $ Set.member (prettyPrint name) excludedSet
          -> return [DataD cxt1 name tt k cons $ updateDerivs derivs]
        NewtypeD cxt1 name tt k cons derivs
          | not $ Set.member (prettyPrint name) excludedSet
          -> return [NewtypeD cxt1 name tt k cons $ updateDerivs derivs]
#endif
        _ -> return [dec]


mkDebugName n@(c:_)
  | isAlpha c || c == '_' = n ++ "_debug"
  | otherwise = n ++ "??"
mkDebugName [] = error "unreachable: impossible"

adjustInnerSigD (SigD n ty) = SigD n (adjustTy ty)
adjustInnerSigD other       = other

----------------------------------------------------------
-- With a little help from Neil Mitchell's debug package
prettyPrint :: (Data a, Ppr a) => a -> String
prettyPrint = pprint . transformBi f
    where f (Name x _) = Name x NameS -- avoid nasty qualifications

-- Add a wildcard for Observable a
adjustTy (ForallT vars ctxt typ) =
    ForallT vars (delete WildCardT ctxt ++ [WildCardT]) typ
adjustTy other = adjustTy $ ForallT [] [] other

-- Tyvar renaming is a work around for http://ghc.haskell.org/trac/ghc/ticket/14643
renameForallTyVars (ForallT vars ctxt typ) = do
  let allVarNames = case vars of
                      []-> snub $ universeBi ctxt ++ universeBi typ
                      _  -> map getVarNameFromTyBndr vars
  vv <- Map.fromList <$> mapM (\v -> (v,) <$> newName (pprint v)) allVarNames
  let Just renamedCtxt = transformBiM (applyRenaming vv) ctxt
      Just renamedTyp  = transformBiM (applyRenaming vv) typ
      Just renamedVars = mapM (applyRenamingToTyBndr vv) vars
  return $
    ForallT renamedVars renamedCtxt renamedTyp

renameForallTyVars other = return other

applyRenaming nn (VarT n) = VarT <$> Map.lookup n nn
applyRenaming _ other     = return other

getVarNameFromTyBndr (PlainTV n)    = n
getVarNameFromTyBndr (KindedTV n _) = n

applyRenamingToTyBndr vv (PlainTV n)    = PlainTV <$> Map.lookup n vv
applyRenamingToTyBndr vv (KindedTV n k) = (`KindedTV` k) <$> Map.lookup n vv

hasRankNTypes (ForallT vars ctxt typ) = hasRankNTypes' typ
hasRankNTypes typ                     = hasRankNTypes' typ
hasRankNTypes' typ = not $ null [ () | ForallT{} <- universe typ]

adjustValD decl@ValD{} = transformBi adjustPat decl
adjustValD other       = other

adjustPat (VarP x) = ViewP (VarE 'observe `AppE` toLit x) (VarP x)
adjustPat x        = x

toLit (Name (OccName x) _) = LitE $ StringL x
