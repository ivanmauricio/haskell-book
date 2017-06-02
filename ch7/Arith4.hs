module Arith4 where

roundTrip :: Num b => (Show a, Read b) => a -> b
roundTrip  = read.show

main = do
  print (roundTrip 4)
  print (id 4)
