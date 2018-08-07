module Phone (number) where

import Data.Char (ord)

number :: String -> Maybe String
number xs = case validate $ cleaned xs of
  [True, _, _, True, _, _, _, _, _, _] -> Just (cleaned xs)
  otherwise                            -> Nothing
  where
    withoutCC ('1' : p) = p
    withoutCC p         = p
    cleaned p           = withoutCC $ filter (\c -> not $ c `elem` ['-', '.', '+', ' ', '(', ')']) p
    validate p          = map (isHighDigit) p
    isHighDigit c       = ord c > ord '1'
