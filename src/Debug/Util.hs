-- | Module containing functions required by test code. Not part of the public interface. 
module Debug.Util(
    -- * Exported for tests only
    removeLet,
    removeExtraDigits
    ) where

import Data.List.Extra        

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