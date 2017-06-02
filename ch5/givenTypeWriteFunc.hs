module GivenTypeWriteFunc where

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r x = tail x

s :: [b] -> [b]
s y = take 1 y

co :: (b -> c) -> (a -> b) -> a -> c
co b2c a2b x = b2c (a2b x)

a :: (a -> x) -> a -> a
a a2x n = n

a' :: (a -> b) -> a -> b
a' a2b x = a2b x
