module RunLength (decode, encode) where

import Data.List
import Data.Char

decode :: String -> String
decode "" = ""
decode text = replaceNumbersAndConcat $ separateNumbers text

encode :: String -> String
encode text = concatMap foldRepeat $ group text

foldRepeat :: String -> String
-- replace list like "aaaaa" with "5a"
foldRepeat a
  |length a == 1 = a
  |otherwise = show (length a) ++ take 1 a

replace :: (a -> Bool) -> [a] -> a -> [a]
-- replaces elements that match the function
replace _ [] _= []
replace f (x:xs) y = if f x then y : replace f xs y else x : replace f xs y

separateNumbers :: String -> [(Int, String)]
-- splits string like "3ab4c" into tuple of lists -- ([3,"","",4,""], ["","a","b","","c"])
separateNumbers x = zip (numbersList x) (lettersList x)
  where numbersList x = map digitsToNumbers $ digitsList x
        lettersList x = splitBy '_' $ lettersString x
        digitsList x = if null (splitBy '_' (digitsString x)) then ["1"] else splitBy '_' $ digitsString x
        lettersString x = replace isDigit x '_' -- replace all digits with '_'
        digitsString x = replace isAlphaSpace x '_' --replace all letters and spaces with '_'
        isAlphaSpace x = isAlpha x || isSpace x
        digitsToNumbers xs = foldl(\acc x -> acc*10 + digitToInt x) 0 xs -- replace list of digits with appropriate number, like ['1', '3'] -> 13

splitBy :: Char -> String -> [String]
--splits string by specified delimiter
splitBy p s =  case dropWhile (==p) s of
                      "" -> []
                      s' -> w : splitBy p s''
                            where (w, s'') = break (==p) s'

replaceNumbersAndConcat :: [(Int, String)] -> String
-- concats list of tuples like (5,"abc") to string like "aaaaabc"
replaceNumbersAndConcat = concatMap join
  where join (a, b) = replicate a (head b) ++ tail b
