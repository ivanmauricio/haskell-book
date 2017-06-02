module Ch9.Filter where

myFilter :: [Char] -> [[Char]]
myFilter =
  (filter prd) . words

prd :: [Char] -> Bool
prd word
  | word == "an" = False
  | word == "a"  = False
  | otherwise    = True
