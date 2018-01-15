{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Debug
  ( Observable(..)
  , observe
  , debugRun
  , getDebugTrace
  , HoedOptions(..)
  , defaultHoedOptions
  , debug
  , debug'
  , Config(..)
  , debugViewTrace
  , debugJSONTrace
  , debugPrintTrace
  , debugSaveTrace
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Graph.Libgraph
import qualified Data.HashMap.Monoidal as HM
import qualified Data.HashMap.Strict   as HMS
import qualified Data.HashSet          as Set
import Data.Hashable
import Data.List
import Data.List.Extra
import Data.Maybe
import Debug.Record as D (DebugTrace(..), Function(..), CallData(..), debugViewTrace, debugJSONTrace, debugSaveTrace, debugPrintTrace)
import Debug.Hoed hiding (runO)
import Debug.Hoed.CompTree
import Debug.Hoed.Render
import GHC.Generics
import GHC.Exts (IsList(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

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
  let !compTree = hoedCompTree hoedAnalysis
  return $ convert compTree

type a :-> b = HM.MonoidalHashMap a b

data HoedFunctionKey = HoedFunctionKey
  { label :: !String
  , arity :: !Int
  , clauses :: !([String])
  }
  deriving (Eq, Generic, Hashable)

type HoedCallKey = Int

data HoedCallDetails = HoedCallDetails
  { argValues
  , clauseValues :: !([String])
  , result :: !String
  , depends, parents :: ![HoedCallKey]
  } deriving (Eq, Generic, Hashable)

instance Hashable Vertex where
  hashWithSalt s RootVertex = s `hashWithSalt` (-1 :: Int)
  hashWithSalt s (Vertex cs _) = s `hashWithSalt` cs
instance Hashable CompStmt where
  hashWithSalt s cs = hashWithSalt s (stmtIdentifier cs)

---------------------------------------------------------------------------
-- Cached pred and succ relationships

data AnnotatedCompTree = AnnotatedCompTree
  { compTree :: CompTree
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
hoedCallValues :: HoedCallDetails -> [String]
hoedCallValues HoedCallDetails{..} = result : (argValues ++ clauseValues)

getRelatives rel v =
      [ stmtIdentifier
        | v'@Vertex {vertexStmt = CompStmt {stmtIdentifier, stmtDetails = StmtLam {}}} <- rel v
      ] ++
      [ callKey
        | v'@Vertex {vertexStmt = CompStmt {stmtDetails = StmtCon {}}} <- rel v
        , callKey <- getRelatives rel v'
      ]

extractHoedCall :: AnnotatedCompTree -> Vertex -> Maybe (HoedFunctionKey, HoedCallKey, HoedCallDetails)
extractHoedCall hoedCompTree v@Vertex {vertexStmt = c@CompStmt {stmtDetails = StmtLam {..}, ..}} =
  Just
    ( HoedFunctionKey stmtLabel (length stmtLamArgs) (map fst clauses)
    , stmtIdentifier
    , HoedCallDetails stmtLamArgs (map snd clauses) stmtLamRes depends parents)
  where
    clauses =
      [ (stmtLabel, stmtCon)
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
    hoedFunctionCalls :: HoedFunctionKey :-> [(HoedCallKey, HoedCallDetails)]
    hoedFunctionCalls =
      HM.fromList
        [ (fnKey, [(callKey, callDetails)])
        | Just (fnKey, callKey, callDetails) <-
            map (extractHoedCall (annotateCompTree hoedCompTree)) (vertices hoedCompTree)
        ]
    sortedFunctionCalls =
      sortOn (\(x, _) -> (label x, arity x)) $ toList hoedFunctionCalls

    functions =
      [ Function {..}
      | (HoedFunctionKey {..}, _) <- sortedFunctionCalls
      , let funResult = "$result"
      , let funArguments = map (\i -> "$arg" ++ show i) [1 .. arity] ++ clauses
      -- HACK Expects a multiline label with the function name in the first line, and the code afterwards
      , let funName:linesSource = lines label
      , let funSource = unlines linesSource
      ]
    variables :: [String]
    variables =
      snub $
      foldMap (foldMap (hoedCallValues . snd) . toList) hoedFunctionCalls

    lookupFunctionIndex =
      fromMaybe (error "bug in convert: lookupFunctionIndex") .
      (`HMS.lookup` HMS.fromList (zip (map fst sortedFunctionCalls) [0 ..]))

    lookupVariableIndex =
      fromMaybe (error "bug in convert: lookupVariableIndex") .
      (`HMS.lookup` HMS.fromList (zip variables [0 ..]))

    lookupCallIndex =
      fromMaybe (error "bug in convert: lookupCallIndex") .
      (`HMS.lookup` HMS.fromList (zip (map fst callsTable) [0 ..]))

    callsTable =
      [ (callId, CallData {..})
      | (k@HoedFunctionKey {..}, calls) <- toList sortedFunctionCalls
      , (callId, HoedCallDetails {..}) <- toList calls
      , let callFunctionId = lookupFunctionIndex k
      , let callVals =
              map (second lookupVariableIndex) $
              ("$result", result) :
              zipWith (\i v -> ("$arg" ++ show i, v)) [1 ..] argValues ++
              zip clauses clauseValues
      , let callDepends = map lookupCallIndex depends
      , let callParents = map lookupCallIndex parents
      ]

    calls = map snd callsTable

snub :: Ord a => [a] -> [a]
snub = map head . group . sort

----------------------------------------------------------------------------
-- Template Haskell

data Config = Config
  { generateGenericInstances, generateObservableInstances :: Bool
  , excludeFromInstanceGeneration :: [String]
  }

debug = debug' (Config False False [])

-- | A @TemplateHaskell@ wrapper to convert normal functions into traced functions.
debug' :: Config -> Q [Dec] -> Q [Dec]
debug' Config{..} q = do
  missing <-
    filterM
      (fmap not . isExtEnabled)
      ([ ViewPatterns
       , PartialTypeSignatures
       , ExtendedDefaultRules
       , FlexibleContexts
       ] ++
       [DeriveAnyClass | generateObservableInstances] ++
       [DerivingStrategies | generateObservableInstances] ++
       [DeriveGeneric | generateGenericInstances]
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
  fmap concat $
    forM decs $ \dec ->
      case dec of
        ValD (VarP n) b clauses
          | checkSig n -> do
            let Just n' = lookup n names
                nb = nameBase n
            -- HACK We embed the source code of the function in the label,
            --      which is then unpacked by 'convert'
                label = (nb ++ "\n" ++ prettyPrint dec)
            newDecl <-
              funD n [clause [] (normalB [|observe label $(varE n')|]) []]
            let clauses' = transformBi adjustValD clauses
            return [newDecl, ValD (VarP n') b clauses']
        FunD n clauses
          | checkSig n -> do
            let Just n' = lookup n names
                nb = nameBase n
            -- HACK We embed the source code of the function in the label,
            --      which is then unpacked by 'convert'
                label = (nb ++ "\n" ++ prettyPrint dec)
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
        DataD cxt1 name tt k cons derivs
          | generateGenericInstances
          , not $ Set.member (prettyPrint name) excludedSet
          , [] <- filterDerivingClausesByName ''Generic derivs
          , derivGen <- DerivClause (Just StockStrategy) [ConT ''Generic]
          , derivObs <- DerivClause (Just AnyclassStrategy) [ConT ''Observable]
          -> return [DataD cxt1 name tt k cons (derivGen : derivObs : derivs)]
        DataD cxt1 name tt k cons derivs
          | generateObservableInstances
          , not $ Set.member (prettyPrint name) excludedSet
          , _:_ <- filterDerivingClausesByName ''Generic derivs
          , []  <- filterDerivingClausesByName ''Observable derivs
          , derivObs <- DerivClause (Just AnyclassStrategy) [ConT ''Observable]
          -> return [DataD cxt1 name tt k cons (derivObs : derivs)]
        NewtypeD cxt1 name tt k cons derivs
          | generateGenericInstances
          , not $ Set.member (prettyPrint name) excludedSet
          , [] <- filterDerivingClausesByName ''Generic derivs
          , derivGen <- DerivClause (Just StockStrategy) [ConT ''Generic]
          , derivObs <- DerivClause (Just AnyclassStrategy) [ConT ''Observable]
          -> return [NewtypeD cxt1 name tt k cons (derivGen : derivObs : derivs)]
        NewtypeD cxt1 name tt k cons derivs
          | generateObservableInstances
          , not $ Set.member (prettyPrint name) excludedSet
          , _:_ <- filterDerivingClausesByName ''Generic derivs
          , []  <- filterDerivingClausesByName ''Observable derivs
          , derivObs <- DerivClause (Just AnyclassStrategy) [ConT ''Observable]
          -> return [NewtypeD cxt1 name tt k cons (derivObs : derivs)]
        _ -> return [dec]

filterDerivingClausesByName n' derivs = [ it | it@(DerivClause _ preds) <- derivs , ConT n <- preds , n == n']

mkDebugName n@(c:_)
  | isAlpha c || c == '_' = n ++ "_debug"
  | otherwise = n ++ "??"

adjustInnerSigD (SigD n ty) = SigD n (adjustTy ty)
adjustInnerSigD other = other

----------------------------------------------------------
-- With a little help from Neil Mitchell's debug package
prettyPrint :: (Data a, Ppr a) => a -> String
prettyPrint = pprint . transformBi f
    where f (Name x _) = Name x NameS -- avoid nasty qualifications

instance Hashable Name
instance Hashable NameSpace
instance Hashable NameFlavour
instance Hashable ModName
instance Hashable OccName
instance Hashable PkgName

-- Add a wildcard for Observable a
-- Tyvar renaming is a work around for http://ghc.haskell.org/trac/ghc/ticket/14643
adjustTy (ForallT vars ctxt typ) =
    ForallT vars (delete WildCardT ctxt ++ [WildCardT]) typ
adjustTy other = adjustTy $ ForallT [] [] other

renameForallTyVars (ForallT vars ctxt typ) = do
  let allVarNames = case vars of
                      []-> snub $ universeBi ctxt ++ universeBi typ
                      _ -> map getVarNameFromTyBndr vars
  vv <- HMS.fromList <$> mapM (\v -> (v,) <$> newName (pprint v)) allVarNames
  let Just renamedCtxt = transformBiM (applyRenaming vv) ctxt
      Just renamedTyp  = transformBiM (applyRenaming vv) typ
      Just renamedVars = mapM (applyRenamingToTyBndr vv) vars
  return $
    ForallT renamedVars renamedCtxt renamedTyp

renameForallTyVars other = return other

applyRenaming nn (VarT n) = VarT <$> HMS.lookup n nn
applyRenaming _ other = return other

getVarNameFromTyBndr (PlainTV n) = n
getVarNameFromTyBndr (KindedTV n _) = n

applyRenamingToTyBndr vv (PlainTV n) = plainTV <$> HMS.lookup n vv
applyRenamingToTyBndr vv (KindedTV n k) = (`KindedTV` k) <$> HMS.lookup n vv

hasRankNTypes (ForallT vars ctxt typ) = hasRankNTypes' typ
hasRankNTypes typ = hasRankNTypes' typ
hasRankNTypes' typ = not $ null [ () | ForallT{} <- universe typ]

adjustValD decl@ValD{} = transformBi adjustPat decl
adjustValD other       = other

adjustPat (VarP x) = ViewP (VarE 'observe `AppE` toLit x) (VarP x)
adjustPat x        = x

toLit (Name (OccName x) _) = LitE $ StringL x
