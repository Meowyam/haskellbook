module ListApp where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil
  = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms
-- of concat' and fmap

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> xs = append (f <$> xs) (fs <*> xs)

instance Monoid (List a) where
  mempty = Nil

instance Semigroup (List a) where
  Nil <> l = l
  l <> Nil = l
  (Cons x xs) <> l = Cons x (xs <> l)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [
      (1, return Nil),
      (1, Cons <$> arbitrary <*> arbitrary)
    ]

-- EqProp is from the checkers library
instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main =
  let ls = Cons ("b", "w", 1 :: Int) Nil
  in quickBatch (applicative ls)
