module RunLength (decode, encode) where

import Data.Char (isAlpha, isDigit, isNumber)
import Data.List (group, groupBy)

decode :: String -> String
decode []   = []
decode text = (part text) ++ decode (rest text)
  where
    number [] = 1
    number cs = read cs :: Int
    digits xs = takeWhile isDigit xs
    letter xs = head $ drop (length $ digits xs) xs 
    part   xs = replicate (number $ digits xs) $ letter xs
    rest   xs = drop ((length $ digits xs) + 1) xs
    

encode :: String -> String
encode text = concat $ map (\xs -> (len xs) ++ (head xs : [])) $ group text
  where
    len [x] = ""
    len xs  = show $ length xs
