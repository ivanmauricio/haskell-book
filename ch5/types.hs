module Types where

data Woot

data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (x, y) = (x, x)

h :: (Blah, Woot) -> (Blah, Blah)
h (x, y) = (f y, x)

r :: Int -> String
r = undefined

s :: String -> Char
s = undefined

t :: Int -> Char
t x = s (r x)

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w (q x)

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (a, b) = (xz a, yz b)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge x2y y2tupWZ a = fst (y2tupWZ (x2y a))
