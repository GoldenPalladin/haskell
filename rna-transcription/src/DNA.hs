module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA x =
  if filter (`elem` ['C','G','T','A']) x == x
  then Just (map toRNAamin x)
  else Nothing
  where
    toRNAamin amin = case amin of
      'C' -> 'G'
      'G' -> 'C'
      'T' -> 'A'
      'A' -> 'U'
      _ -> ' '
