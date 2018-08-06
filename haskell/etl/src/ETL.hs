module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map, fromListWith, toList)

transform :: Map Int String -> Map Char Int
transform scores = fromListWith (+) $ split $ lower $ toList scores
  where
    lower m = map (\(i, xs) -> (map toLower xs, i)) m
    split l = concatMap (\(xs, i) -> map (\c -> (c, i)) xs) l
    
