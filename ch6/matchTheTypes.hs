import Data.List

i :: Num a => a
i = 1

f :: Fractional a => a
f = 1.0

g :: RealFrac a => a
g = 1.0

freud :: Ord a => a -> a 
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int

sigmund :: Int -> Int
-- sigmund :: Num a => a -> a 
-- This does not work because myX is of concrete type Int. The type cannot become more
-- generalised in the type signature.
sigmund x = myX

--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

--young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
--signifier :: Ord a => [a] -> a
--This does not work from same reason as above. mySort spits out a [Char] which is
--more specific than the type we are trying to assing to it (Ord a => a)
signifier xs = head (mySort xs)
