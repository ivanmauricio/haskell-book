module Ch10.Database where

import Data.Time

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

-- foldl (-) 0 [1..3]
-- foldl (-) ((-) 0 1) [2,3]
-- foldl (-) ((-) ((-) 0 1) 2) [3]
-- foldl (-) ((-) ((-) ((-) 0 1) 2) 3) []
-- ((-) ((-) ((-) 0 1) 2) 3)
-- ((-) ((-) (-1) 2) 3)
-- ((-) (-3) 3)
-- (-6)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

-- foldr (-) 0 [1..3]
-- (-) 1 (foldr (-) 0 [2..3])
-- (-) 1 ((-) 2 (foldr (-) 0 [3]))
-- (-) 1 ((-) 2 ((-) 3 (foldr (-) 0 [])))
-- (-) 1 ((-) 2 ((-) 3 (0)))


data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

(.>) = flip (.)
(|>) x f = f x

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 4
  , DbNumber 9
  , DbNumber 9
  , DbDate (UTCTime (fromGregorian 1931 5 1) (secondsToDiffTime 34123))
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
  foldr grabDate []
  where
    grabDate (DbDate date) acc = date : acc
    grabDate _ acc = acc

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' =
  foldl grabDate []
  where
    grabDate acc (DbDate date) = acc ++ [date]
    grabDate acc _ = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =
  foldr grabDbNumber []
  where
    grabDbNumber (DbNumber number) acc = number : acc
    grabDbNumber _ acc = acc

filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' =
  foldl grabDbNumber []
  where
    grabDbNumber acc (DbNumber number) = acc ++ [number]
    grabDbNumber acc _ = acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent =
  filterDbDate
  .> foldr getRecent (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 34123))
  where
    getRecent item acc =
      case compare item acc of
        GT -> item
        _ -> acc

mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' =
  filterDbDate
  .> foldl getRecent (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 34123))
  where
    getRecent acc item =
      case compare item acc of
        GT -> item
        _ -> acc

sumDb :: [DatabaseItem] -> Integer
sumDb =
  filterDbNumber
  .> foldr (+) 0

avgDb :: [DatabaseItem] -> Double
avgDb db =
  if db == [] then 0 else (fromIntegral total) / n
  where
    (total, n) = foldr (\num (subTotal, count) -> (num + subTotal, count + 1))
                       (0, 0)
                       (filterDbNumber db)

avgDb' :: [DatabaseItem] -> Double
avgDb' db =
  if db == [] then 0 else (fromIntegral total) / n
  where
    (total, n) = foldl (\(subTotal, count) num -> (num + subTotal, count + 1))
                       (0, 0)
                       (filterDbNumber db)
