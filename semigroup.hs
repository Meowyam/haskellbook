import Data.Monoid
import Data.Semigroup
import System.IO
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial 

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

--identassoc

newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

type IdentAssoc =
  Identity Trivial -> Identity Trivial -> Identity Trivial -> Bool

-- datatwo

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two x y) = Two (a <> x) (b <> y)

type TwoTriv = Two Trivial Trivial

type TwoAssoc =
  TwoTriv -> TwoTriv -> TwoTriv -> Bool

-- datathree

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Semigroup a, Semigroup b, Semigroup c)
  => Semigroup (Three a b c) where
  (Three a b c) <> (Three x y z) = Three (a <> x) (b <> y) (c <> z)

type ThreeTriv = Three Trivial Trivial Trivial

type ThreeAssoc =
  ThreeTriv -> ThreeTriv -> ThreeTriv -> Bool

--and four is the same
-- boolconj

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = 
    frequency [ (1, return (BoolConj True))
              , (1, return (BoolConj False)) ]

instance Semigroup Bool where
  True <> True = True
  _ <> False = False
  False <> _ = False

instance Semigroup BoolConj where
    BoolConj a <> BoolConj b = BoolConj (a <> b) 

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

--
main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentAssoc) 
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
