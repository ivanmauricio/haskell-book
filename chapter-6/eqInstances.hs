data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn v) (TisAn v') = v == v'

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a a') (Two b b') = a == b && a' == b'

data StringOrInt =
    TisAnInt   Int
  | TisAString String deriving Show

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt a') = a == a'
  (==) (TisAString a) (TisAString a') = a == a'
  (==) _ _ = False

data Pair a =
  Pair a a deriving Show

instance Eq a => Eq (Pair a) where
  (==) (Pair v t) (Pair v' t') = v == v' && t == t'

data Tuple a b =
  Tuple a b deriving Show

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple v v') (Tuple t t') = v == t && v' == t'

data Which a =
    ThisOne a
  | ThatOne a deriving Show

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne v) (ThisOne t) = v == t
  (==) (ThatOne v) (ThatOne t) = v == t
  (==) _ _ = False

data EitherOr a b =
    Hello a
  | Goodbye b deriving Show

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello v) (Hello t) = v == t
  (==) (Goodbye v) (Goodbye t) = v == t
  (==) _ _ = False
