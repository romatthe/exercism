module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise              = Just (hamming xs ys)
  where
    hamming xs ys = length $ filter (\x -> x == False) $ different xs ys
    different xs ys = zipWith (==) xs ys 
