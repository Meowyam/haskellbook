-- fmap :: (a -> b) -> f a -> f b
-- (=<<) :: (a -> m b) -> m a -> m b
-- traverse :: (a -> f b) -> t a -> f (t b)
-- traverse f = sequenceA . fmap f

import Data.Monoid
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative

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

instance (Eq a, Eq b) => EqProp (Constant a b) where
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
data Three a b c =
  Three a b c
  deriving (Eq, Ord, Show)

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z) 

-- traverse :: (a -> f b) -> t a -> f (t b)
instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z 

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

--
data Pair a b =
  Pair a b
  deriving (Eq, Ord, Show)

instance Foldable (Pair a) where
  foldMap f (Pair x y) = f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y) 

instance Traversable (Pair a) where
  traverse f (Pair x y) = Pair x <$> f y 

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

--
data Big a b =
  Big a b b
  deriving (Eq, Ord, Show)

instance Foldable (Big a) where
  foldMap f (Big x y z) = (f y) <> (f z)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Big x y y

--liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
instance Traversable (Big a) where
  traverse f (Big x y z) = liftA2 (Big x) (f y) (f z) 

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

--
data Bigger a b =
  Bigger a b b b
  deriving (Eq, Ord, Show)

instance Foldable (Bigger a) where
  foldMap f (Bigger w x y z) = (f x) <> (f y) <> (f z)

instance Functor (Bigger a) where
  fmap f (Bigger w x y z) = Bigger w (f x ) (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Bigger x y y y

--liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
instance Traversable (Bigger a) where
  traverse f (Bigger w x y z) = liftA3 (Bigger w) (f x) (f y) (f z) 

instance (Eq a, Eq b) => EqProp (Bigger a b) where
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
  let threetrig :: Three Testtype Testtype Testtype
      threetrig = undefined
  let pairtrig :: Pair Testtype Testtype 
      pairtrig = undefined
  let bigtrig :: Big Testtype Testtype 
      bigtrig = undefined
  let biggertrig :: Bigger Testtype Testtype 
      biggertrig = undefined
  quickBatch $ traversable idtrig
  quickBatch $ traversable constrig
  quickBatch $ traversable optrig
  quickBatch $ traversable lstrig
  quickBatch $ traversable threetrig
  quickBatch $ traversable pairtrig
  quickBatch $ traversable bigtrig
  quickBatch $ traversable biggertrig

