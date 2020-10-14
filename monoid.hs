import Data.Monoid
import Data.Semigroup
import System.IO
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial 

instance Monoid Trivial where
  mempty = Trivial 
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial 

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a =
  (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a =
  (mempty <> a) == a


-- identity a

newtype Identity a =
  Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity a) (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentAssoc =
  Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool

--

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentAssoc)
  quickCheck (monoidLeftIdentity :: Identity (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Identity (Sum Int) -> Bool)
