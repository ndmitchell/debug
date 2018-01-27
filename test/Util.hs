{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Util(equivalentTrace) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.DebugTrace

equivalentTrace :: DebugTrace -> DebugTrace -> Bool
equivalentTrace tr1 tr2 =
  Set.fromList (functions tr1)  == Set.fromList (functions tr2) &&
  Set.fromList (variables tr1)  == Set.fromList (variables tr2) &&
  Set.fromList (fleshCalls tr1) == Set.fromList (fleshCalls tr2)

data FleshedCallData = FleshedCallData
  { function :: Function
  , depends, parents :: Set FleshedCallData
  , vals :: Set (Text, Text)
  }
  deriving (Eq, Ord)

fleshCalls :: DebugTrace -> [FleshedCallData]
fleshCalls DebugTrace{..} = map fleshCall calls
  where
    result = map fleshCall calls
    sansStack call = call{depends = [], parents = []}
    fleshCall CallData{..} = FleshedCallData{..} where
      function = functions !! callFunctionId
      depends  = Set.fromList $ map (sansStack . (result !!)) callDepends
      parents  = Set.fromList $ map (sansStack . (result !!)) callParents
      vals     = Set.fromList [ (label, variables !! v) | (label,v) <- callVals ]
