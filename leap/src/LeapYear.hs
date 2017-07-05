module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year =
  let yearDivisibleBy = divisible year
  in (yearDivisibleBy 400) || (yearDivisibleBy 4 && not (yearDivisibleBy 100))


divRest :: Integer -> Integer -> Integer
divRest x y = x - y * (div x y)

divisible :: Integer -> Integer -> Bool
divisible x y = case divRest x y of
  0 -> True
  _ -> False
