module Acronym (abbreviate) where
import Data.Char (isUpper, isAlpha, isSpace, toUpper)

abbreviate :: String -> String
abbreviate = concatMap processWord.words.filter isAlphaSpace.replaceHyphen
  where isAlphaSpace x = isAlpha x || isSpace x
        replaceHyphen = map (\x-> if x=='-' then ' ' else x)
        processWord x
          | (upF x == x)||(upF x == "") = [toUpper (head x)]
          | otherwise = upF x
        upF = filter isUpper
