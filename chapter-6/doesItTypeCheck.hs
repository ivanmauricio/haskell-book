data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving (Show, Ord, Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                 then Blah
                 else x

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving Show

s1 :: String -> Sentence
s1 = Sentence "dogs" "drool"
s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs" 

data Rocks =
  Rocks String deriving (Eq, Show, Ord)

data Yeah =
  Yeah Bool deriving (Eq, Show, Ord)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show, Ord)

{-phew = Papu "chases" True-}

truth = Papu (Rocks "chomskydoz")
             (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
