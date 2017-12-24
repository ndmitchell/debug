{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Debug.Backend.Hoed
  ( Observable(..)
  , runO
  , Debug.Backend.Hoed.getDebugTrace
  , debug
  ) where

import Data.Bifunctor
import Data.Graph.Libgraph
import qualified Data.HashMap.Monoidal as HM
import qualified Data.HashMap.Strict   as HMS
import Data.Hashable
import Data.List
import Data.Maybe
import Debug.Record as Debug
import Debug.Hoed hiding (runO)
import Debug.Hoed.CompTree
import Debug.Hoed.Render
import Debug.Hoed.TH
import GHC.Generics
import GHC.Exts (IsList(..))

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
      , let funName   = label
      , let funSource = ""
      , let funResult = "$result"
      , let funArguments = map (\i -> "$arg" ++ show i) [1 .. arity] ++ clauses
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
