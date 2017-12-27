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
  , runO
  , getDebugTrace
  , HoedOptions(..)
  , defaultHoedOptions
  , debug
  , debugViewTrace
  , debugJSONTrace
  , debugPrintTrace
  , debugSaveTrace
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Generics.Uniplate.Data
import Data.Graph.Libgraph
import qualified Data.HashMap.Monoidal as HM
import qualified Data.HashMap.Strict   as HMS
import Data.Hashable
import Data.List
import Data.List.Extra
import Data.Maybe
import Debug.Record as D hiding (getDebugTrace)
import Debug.Hoed hiding (runO)
import Debug.Hoed.CompTree
import Debug.Hoed.Render
import GHC.Generics
import GHC.Exts (IsList(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Runs the program collecting a debugging trace and then opens a web browser to inspect it.
runO :: IO () -> IO ()
runO program = getDebugTrace defaultHoedOptions {prettyWidth = 160, verbose = Verbose} program >>= debugViewTrace

-- | Runs the program collecting a debugging trace
getDebugTrace :: HoedOptions -> IO () -> IO DebugTrace
getDebugTrace hoedOptions program =
  convert <$> runO' hoedOptions program

type a :-> b = HM.MonoidalHashMap a b

data HoedFunctionKey = HoedFunctionKey
  { label :: String
  , arity :: Int
  , clauses :: [String]
  }
  deriving (Eq, Generic, Hashable)

type HoedCallKey = Int

data HoedCallDetails = HoedCallDetails
  { argValues
  , clauseValues :: [String]
  , result :: String
  , depends, parents :: [HoedCallKey]
  } deriving (Eq, Generic, Hashable)

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

extractHoedCall :: CompTree -> Vertex -> Maybe (HoedFunctionKey, HoedCallKey, HoedCallDetails)
extractHoedCall hoedCompTree v@Vertex {vertexStmt = c@CompStmt {stmtDetails = StmtLam {..}, ..}} =
  Just
    ( HoedFunctionKey stmtLabel (length stmtLamArgs) (map fst clauses)
    , stmtIdentifier
    , HoedCallDetails stmtLamArgs (map snd clauses) stmtLamRes depends parents)
  where
    clauses =
      [ (stmtLabel, stmtCon)
      | Vertex {vertexStmt = CompStmt {stmtLabel, stmtDetails = StmtCon {..}}} <-
          succs hoedCompTree v
      ]
    depends = snub $ getRelatives (succs hoedCompTree) v
    parents = snub $ getRelatives (preds hoedCompTree) v

extractHoedCall _ _ = Nothing

-- | Convert a 'Hoed' trace to a 'debug' trace
convert :: HoedAnalysis -> DebugTrace
convert HoedAnalysis {..} = DebugTrace {..}
  where
    hoedFunctionCalls :: HoedFunctionKey :-> [(HoedCallKey, HoedCallDetails)]
    hoedFunctionCalls =
      HM.fromList
        [ (fnKey, [(callKey, callDetails)])
        | Just (fnKey, callKey, callDetails) <-
            map (extractHoedCall hoedCompTree) (vertices hoedCompTree)
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

-- | A @TemplateHaskell@ wrapper to convert normal functions into traced functions, and add 'Observable' instances to 'Generic' datatypes.
debug :: Q [Dec] -> Q [Dec]
debug q = do
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
            let clauses' = transformBi adjustValD clauses
            return [newDecl, FunD n' clauses']
        SigD n ty
          | not (hasRankNTypes ty)
          , Just n' <- lookup n names -> do
            dec1 <- adjustSig n ty
            dec2 <- adjustSig n' ty
            return [dec1, dec2]
        _ -> return [dec]

mkDebugName n@(c:_)
  | isAlpha c = n ++ "Debug"
  | otherwise = n ++ "??"

----------------------------------------------------------
-- With a little help from Neil Mitchell's debug package

prettyPrint = pprint . transformBi f
    where f (Name x _) = Name x NameS -- avoid nasty qualifications

-- Add a wildcard for Observable a
adjustSig name (ForallT vars ctxt typ) =
  return $
    SigD name $
    ForallT vars (ctxt ++ [WildCardT]) typ
adjustSig name other = adjustSig name $ ForallT [] [] other

hasRankNTypes (ForallT vars ctxt typ) = hasRankNTypes' typ
hasRankNTypes typ = hasRankNTypes' typ
hasRankNTypes' typ = not $ null [ () | ForallT{} <- universe typ]

adjustValD decl@ValD{} = transformBi adjustPat decl
adjustValD other       = other

adjustPat (VarP x) = ViewP (VarE 'observe `AppE` toLit x) (VarP x)
adjustPat x        = x

toLit (Name (OccName x) _) = LitE $ StringL x
