module Diamond (diamond) where

import Data.Char (isAlpha)

diamond :: Char -> Maybe [String]
diamond char
  | not $ isAlpha char = Nothing
  | otherwise          = Just (map (\c -> mask pos c) rows)  
  where
    chars     = ['A' .. char]
    rows      = init chars ++ reverse chars
    pos       = init (reverse chars) ++ chars
    row       = replicate (length rows) ' '
    mask xs c = map (\x -> if x == c then c else ' ') xs
