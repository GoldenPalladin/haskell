module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just $ sum.map tupDiff $ zip xs ys

tupDiff :: Eq a => (a, a) -> Int
tupDiff (x, y)
  | x==y = 0
  | otherwise = 1
