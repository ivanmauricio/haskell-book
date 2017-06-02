module Ch10.Scan where

fibs :: Num a => [a]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Num a => Int -> a
fibsN x = fibs !! x

fibs20 :: Num a => [a]
fibs20 = take 20 fibs

fibsUnder100 :: (Num a, Ord a) => [a]
fibsUnder100 = takeWhile (< 100) fibs

factorial :: (Num a, Enum a) => [a]
factorial = scanl (*) 1 [2..]

factorialN :: (Num a, Enum a) => Int -> a
factorialN n = factorial !! (n - 1) 
