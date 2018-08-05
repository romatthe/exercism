module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = isDivBy 400 || (isDivBy 4 && not (isDivBy 100))
  where isDivBy n = mod year n == 0
