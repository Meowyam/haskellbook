module ValApp where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a

-- This is different
instance Monoid e =>
        Applicative (Validation e) where
  pure = Success'
  Success' f <*> Success' a = Success' (f a)
  Success' _ <*> Failure' e = Failure' e
  Failure' e <*> Success' _ = Failure' e
  Failure' e <*> Failure' g = Failure' (e <> g) 

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure' <$> arbitrary,
                     Success' <$> arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main =
  let val = Success' ("hi", "hey", "ho") :: Validation String ([Char], [Char], [Char])
  in quickBatch (applicative val)


