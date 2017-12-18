{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
    -- Any moved from GHC.Prim to GHC.Types
    -- so import both and use unused imports to get compatibility

module Debug.Variables(
    Var, varId, varShow,
    Variables, listVariables, newVariables, addVariable
    ) where

import GHC.Types
import GHC.Prim
import Data.List.Extra
import Control.Exception
import System.IO.Unsafe
import Unsafe.Coerce


data Variables = Variables
    Int -- Number in the array
    [(Any, String)] -- Entries, (key a, show a), indexed from [n..0]

data Var = Var Int String -- index into Variables, show a
         deriving (Eq)

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
        Just i -> (vs, Var (n-i-1) $ snd $ xs !! i)
    where
        keyA = unsafeCoerce a
        showA = show a

ptrEqual :: Any -> Any -> Bool
ptrEqual a b = unsafePerformIO $ do
    a <- evaluate a
    b <- evaluate b
    return (tagToEnum# (reallyUnsafePtrEquality# a b) :: Bool)
