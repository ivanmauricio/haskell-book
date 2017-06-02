module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord 0 = "zero"
digitToWord _ = "fool"

digits :: Int -> [Int]
digits n = go n []

wordNumber :: Int -> String
wordNumber n = 
  concat $ intersperse "-" numberList
  where numberList = map digitToWord (digits n) 

go :: Int -> [Int] -> [Int]
go curr acc 
  | (divTen curr) == 0 = (intoList curr) ++ acc       
  | otherwise = go (divTen curr) ((intoList curr) ++ acc)

divTen :: Int -> Int
divTen n = div n 10

intoList :: Int -> [Int]
intoList n = (:[]) $ mod n 10

