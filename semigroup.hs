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

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentAssoc) 
