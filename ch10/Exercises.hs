module Ch10.Exercises where

stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

combinator :: [(Char, Char, Char)]
combinator =
  [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns :: [[Char]]
nouns = [ "car"
        , "dmt"
        , "pipe"
        , "dimension"
        , "universe"
        , "consciousness"
        , "infinity"
        ]

verbs :: [[Char]]
verbs = [ "smoke"
        , "expand"
        , "feel"
        , "connect"
        ]

combinator' :: [([Char], [Char], [Char])]
combinator' = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

seekritFunc :: [Char] -> Double
seekritFunc x =
  (/) (fromIntegral (sum (map length (words x))))
      (fromIntegral (length (words x)))

myOr :: [Bool] -> Bool
myOr = foldr (\bool acc -> if bool then bool else acc) False

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny fn = foldr (\x acc -> if fn x then True else acc) False

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' fn = foldr ((||) . fn) False

myElem :: Eq a => a -> [a] -> Bool
myElem = any . (==)

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = foldr ((||) . (== x)) False

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldr (\item acc -> acc ++ [item]) []

myReverse'' :: [a] -> [a]
myReverse'' = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap fn = foldr ((:) . fn) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fn = foldr test []
  where test item acc = if fn item then item : acc else acc

mySquish :: [[a]] -> [a]
mySquish [] = []
mySquish (x:xs) = x ++ mySquish xs

mySquish' :: [[a]] -> [a]
mySquish' = foldr (++) []

mySquishMap :: (a -> [b]) -> [a] -> [b]
mySquishMap _  [] = []
mySquishMap fn (x:xs) = fn x ++ (mySquishMap fn xs)

mySquishMap' :: (a -> [b]) -> [a] -> [b]
mySquishMap' fn = foldr ((++) . fn) []

squishAgain :: [[a]] -> [a]
squishAgain = mySquishMap' id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy fn (x:xs) = foldl maxBy x xs
  where maxBy acc item = if fn acc item == GT then acc else item

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy fn (x:xs) = foldl minBy x xs
  where minBy acc item = if fn acc item == LT then acc else item
