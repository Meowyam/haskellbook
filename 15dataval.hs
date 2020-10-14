import Data.Semigroup
import Test.QuickCheck hiding (Failure, Success)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial 

instance Arbitrary Trivial where
  arbitrary = return Trivial

data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a =>
  Semigroup (Validation a b) where
   Failure a <> Failure b = Failure (a <> b)
   Success a <> _ = Success a 
   _ <> Success b = Success b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Failure a)),
               (1, return (Success b))]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type ValTriv = Validation Trivial Trivial

type ValAssoc = ValTriv -> ValTriv -> ValTriv -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: ValAssoc)
