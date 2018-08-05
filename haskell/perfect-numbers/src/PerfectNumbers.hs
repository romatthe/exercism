module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n < 1          = Nothing
  | aliquot n == n = Just Perfect
  | aliquot n > n  = Just Abundant
  | aliquot n < n  = Just Deficient 
  where
    aliquot n = sum [x | x <- [1..(n - 1)], rem n x == 0]

