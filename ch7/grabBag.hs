addOneIfOdd :: Int -> Int
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \x -> x + 1

addFive :: Int -> Int -> Int
addFive = \x -> \y -> (if x > y then y else x) + 5

mFlip :: (a -> b -> c) -> b -> a -> c
mFlip f x y = f y x

