module Isogram (isIsogram) where

import Data.Char (toLower)
import Data.List (group, sort)

isIsogram :: String -> Bool
isIsogram xs = not $ any (\(x, c) -> c > 1) $ map (\x -> (head x, length x)) $ group $ sort $ clean xs

clean :: String -> String
clean xs = map toLower $ filter (\c -> not $ c `elem` ['-', ' ']) xs
