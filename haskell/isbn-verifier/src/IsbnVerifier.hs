module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isAlphaNum, isNumber)

isbn :: String -> Bool
isbn xs = case valid xs of
  True  -> (foldl (+) 0 $ zipWith (*) (reverse [1..10]) $ values $ clean xs) `mod` 11 == 0
  False -> False
  where
    clean cs     = filter isAlphaNum cs
    value 'X'    = 10
    value n      = digitToInt n
    values cs    = map value cs
    valid cs     = (validLen $ clean cs) && (validLast $ last $ clean cs) && (validRest $ init $ clean cs)
    validLast c  = c `elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'X']
    validRest cs = all isNumber cs
    validLen cs  = length cs == 10
