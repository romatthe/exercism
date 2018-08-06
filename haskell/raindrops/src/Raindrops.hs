module Raindrops (convert) where

convert :: Int -> String
convert n = case (concat $ map convert factors) of
  "" -> show n
  xs -> xs 
  where
    factors = [x | x <- [1..n], n `rem` x == 0]
    convert x
      | x == 3    = "Pling"
      | x == 5    = "Plang"
      | x == 7    = "Plong"
      | otherwise = ""
