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

instance Semigroup BoolConj where
    BoolConj True <> BoolConj True = BoolConj True 
    BoolConj False <> BoolConj _ = BoolConj False 
    BoolConj _ <> BoolConj False = BoolConj False 

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

-- booldisj

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = 
    frequency [ (1, return (BoolDisj True))
              , (1, return (BoolDisj False)) ]

instance Semigroup BoolDisj where
    BoolDisj False <> BoolDisj False = BoolDisj False 
    BoolDisj True <> BoolDisj _ = BoolDisj True 
    BoolDisj _ <> BoolDisj True = BoolDisj True 

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- or a b

data Or a b =
    Fst a
  | Snd b
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (Fst a))
              , (1, return (Snd b)) ]

instance Semigroup (Or a b) where
  Fst a <> Snd b = Snd b
  Fst a <> Fst b = Fst b
  Snd a <> _ = Snd a

type OrTriv = Or Trivial Trivial

type OrAssoc =
  OrTriv -> OrTriv -> OrTriv -> Bool

-- combine a b
--
main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentAssoc) 
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
