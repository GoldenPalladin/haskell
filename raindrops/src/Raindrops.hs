module Raindrops (convert) where

convert :: Int -> String
convert n = if rain == "" then show n else rain
  where rain = rDrop 3 n ++ rDrop 5 n ++ rDrop 7 n

rDrop :: Int -> Int -> String
rDrop factor x
  | (mod x factor == 0) && (factor == 3) = "Pling"
  | (mod x factor == 0) && (factor == 5) = "Plang"
  | (mod x factor == 0) && (factor == 7) = "Plong"
  | otherwise = ""
