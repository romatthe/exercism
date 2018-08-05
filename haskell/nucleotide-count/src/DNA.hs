module DNA (nucleotideCounts) where

import Data.Map (Map, fromList, unionWith)
import Data.List (group, sort)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs = case invalid xs of
  True  -> Left xs
  False -> Right (count xs)

invalid :: String -> Bool
invalid xs = any (\x -> not $ x `elem` ['G', 'T', 'C', 'A']) xs

count :: String -> Map Char Int
count xs = unionWith (+) base $ fromList $ map (\x -> (head x, length x)) $ group $ sort xs
  where
    base = fromList [('G', 0), ('T', 0), ('C', 0), ('A', 0)]
