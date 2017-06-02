module Exercises where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- appedCatty "woohoo!" => "woops mrow woohoo!"
-- frappe "1" => "1 mrow haha"
-- frappe (appedCatty "2") => "woops mrow 2 mrow haha" 
-- appedCatty (frappe "blue") => "woops mrow blue mrow haha"
-- cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue")) =>
--   "pink mrow haha mrow green mrow woops mrow blue"
-- cattyConny (flippy "Pugs" "are") "awesome" => "are mrow Pugs mrow awesome"

mySum :: (Eq a, Num a) => a -> a
mySum 0 = 0
mySum n = n + mySum (n - 1)

myMult :: (Integral a) => a -> a -> a
myMult x 1 = x
myMult a b = a + myMult a (b - 1) 

{-type Numerator = Integer-}
{-type Denominator = Integer-}
{-type Quotient = Integer-}

{-dividedBy :: Numerator -> Denominator -> Quotient-}

data DividedResult =
    Result (Integer, Integer)
  | DividedByZero deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom
  | (num < 0) && (denom < 0) = go (-num) (-denom) 0
  | (num < 0) = go (-num) denom 0
  | (denom < 0) = go num (-denom) 0
  | denom == 0 = DividedByZero
  | otherwise = go num denom 0
  where go n d count
         | n < d = Result (count, n)
         | otherwise = go (n - d) d (count + 1)

mc91 :: Integral a => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise = mc91 $ mc91 $ x + 11
