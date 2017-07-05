module Pangram (isPangram) where
import Data.Char
import Data.List

isPangram :: String -> Bool
isPangram text = uniqueLiteralsCount (purifyString text) == 26

purifyString :: String -> String
purifyString xs  = sort $ map toLower $ filter isAlpha xs

uniqueLiteralsCount :: String -> Int
uniqueLiteralsCount xs = foldl (\acc x -> acc+1) 0 $ group xs
