module RunLength (decode, encode) where

import Data.Char (isAlpha, isDigit, isNumber)
import Data.List (group, groupBy)

decode :: String -> String
decode text = error ""

encode :: String -> String
encode text = concat $ map (\xs -> (len xs) ++ (head xs : [])) $ group text
  where
    len [x] = ""
    len xs  = show $ length xs
