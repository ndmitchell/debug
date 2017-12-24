{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Debug.Backend.Hoed
  ( Observable(..)
  , runO
  , Debug.Backend.Hoed.getDebugTrace
  , debug
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Generics.Uniplate.Data
import Data.Graph.Libgraph
import qualified Data.HashMap.Monoidal as HM
import qualified Data.HashMap.Strict   as HMS
import Data.Hashable
import Data.List
import Data.List.Extra
import Data.Maybe
import Debug.Record as Debug
import Debug.Hoed hiding (runO)
import Debug.Hoed.CompTree
import Debug.Hoed.Render
import GHC.Generics
import GHC.Exts (IsList(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Runs the program collecting a debugging trace and then opens a web browser to inspect it.
runO :: IO () -> IO ()
runO program = program >> Debug.getDebugTrace >>= debugViewTrace

-- | Runs the program collecting a debugging trace
getDebugTrace :: IO () -> IO DebugTrace
getDebugTrace program = convert <$> runO' defaultHoedOptions program

type a :-> b = HM.MonoidalHashMap a b

data HoedFunctionKey = HoedFunctionKey
  { label :: String
  , arity :: Int
  , clauses :: [String]
  }
  deriving (Eq, Generic, Hashable)

type HoedCallKey = [String] -- argValues

data HoedCallDetails = HoedCallDetails
  { clauseValues :: [String]
  , result :: String
  } deriving (Eq, Generic, Hashable)

hoedCallValues :: HoedCallKey -> HoedCallDetails -> [String]
hoedCallValues argValues HoedCallDetails{..} = result : (argValues ++ clauseValues)

-- | Convert a 'Hoed' trace to a 'debug' trace
convert :: HoedAnalysis -> DebugTrace
convert HoedAnalysis {..} = DebugTrace {..}
  where
    hoedFunctionCalls :: HoedFunctionKey :-> HMS.HashMap HoedCallKey HoedCallDetails
    hoedFunctionCalls =
      HM.fromList
        [ ( HoedFunctionKey stmtLabel (length stmtLamArgs) (map fst clauses)
          , [( stmtLamArgs, HoedCallDetails (map snd clauses) stmtLamRes)]
          )
        | v@Vertex {vertexStmt = c@CompStmt { stmtLabel
                                            , stmtDetails = StmtLam {..}
                                            }} <- vertices hoedCompTree
        , let clauses =
                [ (stmtLabel, stmtCon)
                | Vertex {vertexStmt = CompStmt { stmtLabel
                                                , stmtDetails = StmtCon {..}
                                                }} <- succs hoedCompTree v
                ]
        ]
    sortedFunctionCalls =
      sortOn (\(x,_) -> (label x, arity x)) $ toList hoedFunctionCalls
    functions =
      [ Function {..}
      | (HoedFunctionKey{..}, _) <- sortedFunctionCalls
      , let funResult = "$result"
      , let funArguments = map (\i -> "$arg" ++ show i) [1 .. arity] ++ clauses
      -- HACK Expects a multiline label with the function name in the first line, and the code afterwards
      , let funName : linesSource = lines label
      , let funSource = unlines linesSource
      ]

    variables :: [String]
    variables = snub
              $ foldMap (foldMap (uncurry hoedCallValues) . toList)
                hoedFunctionCalls

    lookupFunctionIndex =
         fromMaybe (error "bug in convert: lookupFunctionIndex")
       . (`HMS.lookup` HMS.fromList (zip (map fst sortedFunctionCalls) [0 ..]))

    lookupVariableIndex =
         fromMaybe (error "bug in convert: lookupVariableIndex")
       . (`HMS.lookup` HMS.fromList (zip variables [0 ..]))

    calls =
      [ CallData {..}
      | (k@HoedFunctionKey{..}, calls) <- toList hoedFunctionCalls
      , (argValues, HoedCallDetails{..}) <- toList calls
      , let callFunctionId = lookupFunctionIndex k
      , let callVals =
              map (second lookupVariableIndex) $
              ("$result", result) :
              zipWith (\i v -> ("$arg" ++ show i, v)) [1..] argValues ++
              zip clauses clauseValues
      ]

snub = map head . group . sort

-- | A @TemplateHaskell@ wrapper to convert a normal function into a traced function.
debug :: Q [Dec] -> Q [Dec]
debug q = do
  decs <- q
  names <- sequence [ (n,) <$> newName(nameBase n ++ "Debug") | FunD n _ <- decs]
  fmap concat $ forM decs $ \dec ->
    case dec of
      FunD n clauses -> do
        let Just n' = lookup n names
            nb = nameBase n
            -- HACK We embed the source code of the function in the label,
            --      which is then unpacked by 'convert'
            label = (nb ++ "\n" ++ prettyPrint dec)
        newDecl <- funD n [clause [] (normalB [| observe label $(varE n')|]) []]
        let clauses' = transformBi adjustValD clauses
        return [newDecl, FunD n' clauses']
      SigD n ty | Just n' <- lookup n names -> do
        dec' <- adjustSig n ty
        return [dec']
      _ ->
        return [dec]

----------------------------------------------------------
-- With a little help from Neil Mitchell's debug package

prettyPrint = pprint . transformBi f
    where f (Name x _) = Name x NameS -- avoid nasty qualifications

-- | List all the type variables of kind * (or do the best you can)
kindStar :: Type -> Q [Name]
-- in Q so we should be able to use 'reify' to do a better job
kindStar t = return $
    nubOrd [x | VarT x <- universe t] \\     -- find all variables
    nubOrd [x | AppT (VarT x) _ <- universe t] -- delete the "obvious" ones

-- try and shove in a "Observable a =>" if we can
adjustSig name (ForallT vars ctxt typ) = do
  vs <- kindStar typ
  return $
    SigD name $
    ForallT vars (nub $ map (AppT (ConT ''Observable) . VarT) vs ++ ctxt) typ
adjustSig name other = adjustSig name $ ForallT [] [] other

adjustValD decl@ValD{} = transformBi adjustPat decl
adjustValD other       = other

adjustPat (VarP x) = ViewP (VarE 'observe `AppE` toLit x) (VarP x)
adjustPat x        = x

toLit (Name (OccName x) _) = LitE $ StringL x
