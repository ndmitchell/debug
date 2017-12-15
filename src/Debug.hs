{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Debug(
    debug
    ) where

import Debug.Record
import Data.List
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Generics.Uniplate.Data


debug :: Q [Dec] -> Q [Dec]
debug q = do
    decs <- q
    let askSig x = find (\case SigD y _ -> x == y; _ -> False) decs
    mapM (adjustDec askSig) decs


adjustDec :: (Name -> Maybe Dec) -> Dec -> Q Dec
-- try and shove in a "Show a =>" if we can
adjustDec askSig (SigD name (ForallT vars ctxt typ)) =
    return $ SigD name $ ForallT vars ([AppT (ConT ''Show) x | x@VarT{} <- universe typ] ++ ctxt) typ
adjustDec askSig (SigD name typ) = adjustDec askSig $ SigD name $ ForallT [] [] typ
adjustDec askSig o@(FunD name clauses@(Clause arity _ _:_)) = do
    inner <- newName "inner"
    tag <- newName "tag"
    args <- sequence [newName $ "arg" ++ show i | i <- [1 .. length arity]]
    let addTag (Clause ps bod inner) = Clause (VarP tag:ps) bod inner
    let clauses2 = map addTag $ transformBi (adjustPat tag) clauses
    let dol (LitE (StringL x)) = LitE $ StringL $ "$" ++ x
    let args2 = [VarE 'var `AppE` VarE tag `AppE` dol (toLit a) `AppE` VarE a | a <- args]
    let info = ConE 'Function `AppE`
            toLit name `AppE`
            LitE (StringL $ prettyPrint $ maybeToList (askSig name) ++ [o]) `AppE`
            ListE (map (dol . toLit) args) `AppE`
            LitE (StringL "$result")
    let body2 = VarE 'var `AppE` VarE tag `AppE` LitE (StringL "$result") `AppE` foldl AppE (VarE inner) (VarE tag : args2)
    let body = VarE 'funInfo `AppE` info `AppE` LamE [VarP tag] body2
    return $ FunD name [Clause (map VarP args) (NormalB body) [FunD inner clauses2]]
adjustDec askSig x = return x

prettyPrint = pprint . transformBi f
    where f (Name x _) = Name x NameS -- avoid nasty qualifications

adjustPat :: Name -> Pat -> Pat
adjustPat tag (VarP x) = ViewP (VarE 'var `AppE` VarE tag `AppE` toLit x) (VarP x)
adjustPat tag x = x


toLit (Name (OccName x) _) = LitE $ StringL x
