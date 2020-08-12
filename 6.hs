--Eq instances

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn x') = x == x' 

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x y)
       (Two x' y') = 
       x == x'
    && y == y'

data StringOrInt =
  TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt x') = x == x'
  (==) (TisAString y) (TisAString y') = y == y'
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq x => Eq (Pair x) where
  (==) (Pair x y) 
       (Pair x' y') =
       x == x'
    && y == y'
    && x == y

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y)
       (Tuple x' y') =
       x == x'
    && y == y'

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _ = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye y) (Goodbye y') = y == y'
  (==) _ _ = False

  --

--I think quotRem takes two integrals and returns a tuple,
--so divides one from another and returns the division result and the remainder

--divMod also seems to do the same thing
--different if negative

checkDivmod = divMod (-9) 4
checkQuotrem = quotRem (-9) 4

--because divMod gives a modulus instead of a remainder, as evidenced from the name
--https://rob.conery.io/2018/08/21/mod-and-remainder-are-not-the-same/
--
--will they work?

--max(length [1,2,3])
--   (length [8, 9, 10, 11, 12])
-- it works, the max is 5 (second result)
--
-- compare (3*4) (3*5); compares 12 < 15
--
-- compare "Julie" True; doesn't work; the latter is a Bool value
-- compare "Julie" "True" works
--
-- (5 + 3) > (3 + 6) ; 8 > 9 which is FALSE
--
-- enumFromThenTo 0 10 100 will give every tenth number
-- enumFromThenTo 'a' 'c' 'z' will give every other letter
--
--
--
-- show is just for human readability NOT for saving file to disk
--
--The Eq class makes equality tests possible
--The type class Ord is a subclass of Eq
--The type of > is 
--  Ord a => a -> a -> Bool
-- x = divMod 16 12 is 16/12 gives a tuple of (1, 4)
-- Integral includes integer and int, but not fractional
--
