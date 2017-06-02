module Ch9.FearfulSymmetry where

notEqual :: Char -> Char ->  Bool
notEqual char = (/= char)

myWords :: [Char] -> [[Char]]
myWords string
  | length string == 0 = []
  | otherwise = takeWhile (notEqual ' ') string : myWords (drop 1 $ dropWhile (notEqual ' ') string)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines string
  | string == "" = []
  | dropWhile (notEqual '\n') string == "" = [string]
  | otherwise = [takeWhile (notEqual '\n') string] ++ myLines (tail $ dropWhile (notEqual '\n') string)

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $ "Are they equal? "
           ++ show (myLines sentences == shouldEqual)
