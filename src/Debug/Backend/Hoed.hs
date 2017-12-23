{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Debug.Backend.Hoed(Observable(..), runO, convert, debug) where

import Data.Graph.Libgraph
import qualified Data.HashMap.Strict as HM
import Data.List.Extra
import Debug.Record
import Debug.Hoed hiding (runO)
import Debug.Hoed.CompTree
import Debug.Hoed.Render
import Debug.Hoed.TH

runO :: IO () -> IO DebugTrace
runO program = convert <$> runO' defaultHoedOptions program

convert :: HoedAnalysis -> DebugTrace
convert HoedAnalysis {..} = DebugTrace {..}
  where
    hoedFunctionCalls :: HM.HashMap (String, Int, [String]) [ [(String, String)] ]
    hoedFunctionCalls =
      HM.fromListWith
        (++)
        [ ( (stmtLabel, length stmtLamArgs, map fst clauses)
          , [   zipWith (\i a-> ("$arg" ++ show i, a)) [1..] stmtLamArgs
             ++ ("$result", stmtLamRes)
             :  clauses
            ]
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
    lookupFunctionIndex =
      (HM.fromList (zip (HM.keys hoedFunctionCalls) [0 ..]) HM.!)

    functions =
      [ Function {..}
      | ((funName, arity, clauses), _) <- HM.toList hoedFunctionCalls
      , let funSource = ""
      , let funResult = "$result"
      , let funArguments = map (\i -> "$arg" ++ show i) [1 .. arity] ++ clauses
      ]

    variables :: [String]
    variables = nubOrd $ map snd $ concat $ concat $ HM.elems hoedFunctionCalls

    lookupVariableIndex = (HM.fromList (zip variables [0 ..]) HM.!)

    calls =
      [ CallData {..}
      | (k@(_, _, clauseNames), calls) <- HM.toList hoedFunctionCalls
      , callDetails <- calls
      , let callFunctionId = lookupFunctionIndex k
      , let callVals = [ (callArg, lookupVariableIndex callValue) | (callArg, callValue) <- callDetails ]
      ]
