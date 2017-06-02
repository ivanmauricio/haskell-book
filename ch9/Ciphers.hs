module Ch9.Ciphers where

import Data.Char

caesar :: Int -> [Char] -> [Char]
caesar amount =
  map $ replaceChar amount

replaceChar :: Int -> Char -> Char
replaceChar amount char
  | elem x lowerRange = chr $ shift amount (minimum lowerRange) x
  | elem x upperRange = chr $ shift amount (minimum upperRange) x
  | otherwise = char
  where x = ord char

loop26 :: Int -> Int
loop26 x
  | x > 26 = (rem x 26)
  | otherwise = x

lowerRange :: [Int]
lowerRange =
  map ord ['a'..'z']

upperRange :: [Int]
upperRange =
  map ord ['A'..'Z']

shift :: Int -> Int -> Int -> Int
shift amount lowerBound x =
  root + loop26 y
  where
    root = lowerBound - 1
    y = (x - root) + amount

uncaesar :: Int -> [Char] -> [Char]
uncaesar offset =
  caesar (26 - offset)
