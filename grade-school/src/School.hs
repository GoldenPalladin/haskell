module School (School, add, empty, grade, sorted) where

import Data.Map (Map)
import qualified Data.Map as M--(Map, notMember, insert, update)
import Data.List

type School = Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student school =
  if M.notMember gradeNum school
    then M.insert gradeNum [student] school
    else M.update (\x -> Just (x ++ [student])) gradeNum school

empty :: School
empty = M.empty

grade :: Int -> School -> [String]
grade  = M.findWithDefault [] 

sorted :: School -> [(Int, [String])]
sorted school = M.toAscList $ M.map sort school
