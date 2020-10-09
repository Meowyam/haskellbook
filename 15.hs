-- the runtime representation of newtype and what it wraps are always identical
--stack ghc --package QuickCheck -- MyProgram.hs
import Data.Monoid
import Test.QuickCheck
import Control.Monad

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a
    => Monoid (Optional a) where
  mempty = Nada


instance Semigroup a
      => Semigroup (Optional a) where
  (Nada) <> (Nada) = Nada
  (Nada) <> (Only a) = Only a
  (Only a) <> (Nada) = Only a
  (Only a) <> (Only b) = Only (a <> b)

-- maybe another monoid

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen

optionalGen :: Arbitrary a => Gen (Optional a)
optionalGen = do
  a <- arbitrary
  frequency [ (1, return Nada), (10, return (Only a)) ]

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  a <- optionalGen
  return (First' a)

instance Semigroup (First' a) where
  (First' Nada) <> (First' Nada) = First' Nada
  (First' Nada) <> (First' (Only b)) = First' (Only b)
  (First' (Only b)) <> (First' Nada) = First' (Only b)
  (First' (Only b)) <> (First' (Only c)) = First' (Only b)

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
  First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a

main :: IO () 
main = do
  quickCheck (monoidAssoc :: FirstMappend) 
  quickCheck (monoidLeftIdentity :: FstId) 
  quickCheck (monoidRightIdentity :: FstId)

-- nonempty
-- data NonEmpty a = a :| [a]
-- guarantees that we always have at least one value of type a, which [a] does not guarantee, as any list might be empty.

