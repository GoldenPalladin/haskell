module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as M--(Map, notMember, insert, update)
import Data.List
import Data.Char

listTransform :: (a, String) -> [(Char, a)]
listTransform (x, y) = zip (map toLower y) (repeat x)

transform :: Ord a => Map a String -> Map Char a
transform legacyData = M.fromList $ sort $ foldl (\acc x->acc ++ listTransform x) [] $ M.toList legacyData
