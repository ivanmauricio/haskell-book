chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f y z = f y == z

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i x = (fromInteger i) + f x


