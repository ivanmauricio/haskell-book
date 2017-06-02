module Ch9.Exercices where

import Data.Char

removeLower :: [Char] -> [Char]
removeLower =
  filter isUpper

capitalize :: [Char] -> [Char]
capitalize (x:xs) =
  toUpper x : xs

capitalize' :: [Char] -> [Char]
capitalize' [] = []
capitalize' (x:xs) =
  toUpper x : capitalize' xs

upperHead :: [Char] -> Char
upperHead =
  head . capitalize

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x:xs) = if x then x else myOr' xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny fn (x:xs) = if fn x then True else myAny fn xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' _ [] = False
myAny' fn (x:xs) = fn x || myAny' fn xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem el (x:xs) = if el == x then True else myElem el xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' el (x:xs) = el == x || myElem' el xs

myElem'' :: Eq a => a -> [a] -> Bool
myElem'' el = myAny' (== el)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (l:ls) = l ++ squish ls

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap fn (x:xs) = fn x ++ squishMap fn xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap fn (x:xs) = fn x : myMap fn xs

flipCons :: [a] -> a -> [a]
flipCons = flip (:)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy fn (x:h:t) =
  case fn x h of
    GT -> myMaximumBy fn (x:t)
    _ -> myMaximumBy fn (h:t)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x:[]) = x
myMinimumBy fn (x:h:t) =
  case fn x h of
    LT -> myMinimumBy fn (x:t)
    _ -> myMinimumBy fn (h:t)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
