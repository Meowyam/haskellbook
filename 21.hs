-- fmap :: (a -> b) -> f a -> f b
-- (=<<) :: (a -> m b) -> m a -> m b
-- traverse :: (a -> f b) -> t a -> f (t b)
-- traverse f = sequenceA . fmap f

import Data.Monoid
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Functor (Identity) where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

--

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Traversable (Constant a) where
  traverse f (Constant x) = Constant <$> pure x

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap f (Constant x) = mempty 

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

--

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Traversable (Optional) where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance Functor (Optional) where
  fmap _ (Nada) = Nada
  fmap f (Yep x) = Yep (f x) 

instance Foldable Optional where
  foldMap _ Nada = mempty 
  foldMap f (Yep x) = f x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency [
      (1, return Nada),
      (1, Yep <$> arbitrary)
    ]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

--

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Traversable (List) where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Functor (List) where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs) 
  fmap _ (Nil) = Nil 

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =
    frequency [
      (1, return Nil),
      (1, Cons <$> arbitrary <*> arbitrary)
    ]

instance Foldable (List) where
  foldMap _ Nil = mempty 
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Eq a => EqProp (List a) where
  (=-=) = eq

--

type Testtype = (Int, [Int], String)

--

main = do
  let idtrig :: Identity Testtype
      idtrig = undefined
  let constrig :: Constant Testtype Testtype
      constrig = undefined
  let optrig :: Optional Testtype
      optrig = undefined
  let lstrig :: List Testtype 
      lstrig = undefined
  quickBatch $ traversable idtrig
  quickBatch $ traversable constrig
  quickBatch $ traversable optrig
  quickBatch $ traversable lstrig

