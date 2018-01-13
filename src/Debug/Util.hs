-- | Module containing functions required by test code. Not part of the public interface. 
module Debug.Util(
    -- * Exported for tests only
    mkLegalInfixVar,
    removeLet,
    removeExtraDigits
    ) where

import Data.List.Extra  
import qualified Data.Map.Strict as M
import Data.Maybe      

-- | Discover the function name inside (possibly nested) let expressions
--   Transform strings of the form "let (var tag "f" -> f) = f x in f_1" into "f'" 
--   Each level of nesting gets a ' (prime) appeneded to the name
removeLet :: String -> String
removeLet str = loop "" str where
   loop suffix s = if "let" `isInfixOf` fst (word1 s) 
        then case stripInfix " = " s of
            Just pair -> loop ('\'' : suffix) (snd pair)
            Nothing -> s    -- this shouldn't happen...
        else fst (word1 s) ++ suffix 

-- | Remove possible _n suffix from discovered function names
removeExtraDigits :: String -> String
removeExtraDigits str = case stripInfixEnd "_" str of
    Just s -> fst s
    Nothing -> str

-- | Trsansform infix operator into a valid variable name
-- | For example "++"" ---> "plus_plus"
-- | This transformed variable is not visible in the UI
mkLegalInfixVar :: String -> String
mkLegalInfixVar s = 
    let f c acc = case M.lookup c opNames of
            Just "" -> acc -- no adl underscores when removing backtics
            Just s -> s ++ "_" ++ acc
            Nothing -> c : acc
        removeTrailing_ x = fromMaybe x $ stripSuffix "_" x
    in removeTrailing_ $ foldr f "" s

-- | Legal variable names for each operator character
opNames :: M.Map Char String
opNames = M.fromList opList where 
    opList = [ ('+', "plus"), ('-', "minus"), ('*', "star"), ('/', "div")
             , ('^', "caret"), ('~', "tilde"), ('%', "percent"), ('&', "amp")
             , ('=', "equals"), ('<', "lt"), ('>', "gt"), ('?', "quest")
             , ('.', "dot"), ('@', "at"), ('#', "hash"), ('!', "bang"), ('|', "bar")
             , ('`', "") -- remove backtics to form variable name
             ]
