data Person = Person Bool deriving Show

--printPerson person = putStrLn (show person)
--doesn't typecheck if nothing has an instance of Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
            | Woot
            deriving (Eq, Show)

-- have to Eq and Show 

settleDown x = if x == Woot
                  then Blah
                  else x

-- only Woot and Blah are acceptable, or you'll get Variable not in scope (of Mood)
-- if you use >, you'll get no instance for Ord Mood

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

--s1 is missing a string, gets the maybe not enough arguments error.

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

--phew = Papu "chases" True
--doesn't typecheck because Papu first arg should be Rocks which has string "chases", and second arg should be Yeah which has bool True

phew = Papu (Rocks "chases") (Yeah True)
-- like this:
truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- I guess that works. papu compare to papu is true
-- but the following doesn't work because Papu has not got instance of Ord
--comparePapus :: Papu -> Papu -> Bool
--comparePapus p p' = p > p'
