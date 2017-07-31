module SumOfMultiples (sumOfMultiples) where

import Data.List (union)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit
  |null factors = 0
  |otherwise = sum $ foldr union [0] $ map (singleMultiplier limit) factors

singleMultiplier :: Integer -> Integer -> [Integer]
singleMultiplier lim factor
  |factor > lim = [0]
  |otherwise = takeWhile (<lim) $ map (factor * ) [1,2..]
