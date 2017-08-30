module Phone (number) where
import Data.Char
number :: String -> Maybe String
number xs = checkLen (filter isDigit xs) >>= checkAECode


checkLen :: String -> Maybe String
checkLen x = case length x of
  10 -> Just x
  11 -> if head x == '1' then Just (tail x) else Nothing
  _ -> Nothing

checkAECode :: String -> Maybe String
checkAECode x
  | digitToInt (head x) <2 = Nothing
  | digitToInt (x!!3) < 2 = Nothing
  | otherwise = Just x
