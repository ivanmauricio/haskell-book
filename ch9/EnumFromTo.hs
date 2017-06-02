module Ch9.EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool True False = []
eftBool False True = [False, True]
eftBool False False = [False]
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd GT _ = []
eftOrd LT GT = [LT, EQ, GT]
eftOrd LT EQ = [LT, EQ]
eftOrd EQ LT = []
eftOrd EQ GT = [EQ, GT]

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x > y = []
  | otherwise = reverse $ eftInt_ x y []

eftInt_ :: Int -> Int -> [Int] -> [Int]
eftInt_ x y acc
  | x == y = (x : acc)  
  | otherwise = eftInt_ (succ x) y (x : acc)

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x > y = []
  | otherwise = [x] ++ (eftChar (succ x) y)

{-eftChar_ :: Char -> Char -> [Char] -> [Char]-}
{-eftChar_ x y acc-}
  {-| x == y = (x : acc)  -}
  {-| otherwise = eftChar_ (succ x) y (x : acc)-}
