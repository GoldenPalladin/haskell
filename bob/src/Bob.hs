module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs
  |isYell a = "Whoa, chill out!"
  |isQuestion a = "Sure."
  |isSilence a = "Fine. Be that way!"
  |otherwise = "Whatever."
  where a = trim xs

trim :: String -> String
trim  = filter (not.isSpace)

isYell :: String -> Bool
isYell xs = map toUpper fxs == fxs && not (null fxs)
  where fxs = filter isAlpha xs

isQuestion :: String -> Bool
isQuestion a
  |null a = False
  |otherwise = last a == '?'

isSilence :: String -> Bool
isSilence a
  | null a = True
  | otherwise = False
