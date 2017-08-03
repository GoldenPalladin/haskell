module DNA (nucleotideCounts) where

import Data.Map (Map, fromList, union)
import Data.List (group, sort)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs =
  if filter (`elem` ['C','G','T','A']) xs /= xs
  then Left "Not a DNA"
  else Right $ union actualMap baseMap
  where baseMap = fromList [('A',0),('C',0),('G',0),('T',0)]
        actualMap = fromList $ map nCount $ group $ sort xs
        nCount y = (head y, length y)
