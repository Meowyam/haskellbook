import Control.Monad

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

bind :: Monad m => (a -> m b) -> m a -> m b

--fmap :: Functor m => (a -> b) -> (m a -> m b)
--fmap (++"a") [m] = ["ma"]
-- join :: Monad m => m (m a) -> m a
-- join ["ma"] = "ma"

bind m a = join (fmap m a)

-- either monad

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a

instance Applicative (Sum a) where
  pure = Second 
  (<*>) (First a) = const (First a)
  (<*>) (Second f) = fmap f

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second b) f = f b

--
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (>>=) :: Monad m
--       => m a -> (a -> m b) -> m b
--
-- kleisli composition
-- (>=>) :: Monad m
--       => (a -> m b) -> (b -> m c) -> a -> m c
--
sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
  getAge "Hello! How old are you? "

-- chapter exercises

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor (Nope) where
  fmap _ (NopeDotJpg) = NopeDotJpg

instance Applicative (Nope) where
  pure _ = NopeDotJpg
  (<*>) NopeDotJpg NopeDotJpg = NopeDotJpg

instance Monad (Nope) where
  return = pure
  (>>=) NopeDotJpg _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

--

data BahEither b a =
    PLeft a
  | PRight b
  deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap f (PLeft a) = PLeft (f a)
  fmap _ (PRight b) = PRight b

instance (Monoid b) => Applicative (BahEither b) where
  pure = PLeft 
  (<*>) (PRight b) _ = PRight b
  (<*>) _ (PRight b) = PRight b
  (<*>) (PLeft f) (PLeft a) = PLeft (f a)

instance (Monoid b) => Monad (BahEither b) where
  return = pure
  (>>=) (PLeft a) f = f a
  (>>=) (PRight b) _ = PRight b

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(2, return $ PLeft a),
               (1, return $ PRight b)]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

instance (Semigroup b, Semigroup a)
  => Semigroup (BahEither b a) where
  (PLeft a) <> (PLeft a') = PLeft (a <> a')
  (PRight b) <> (PRight b') = PRight (b <> b')
  (PRight b) <> _ = PRight b
  _ <> (PRight b) = PRight b

instance (Monoid b, Monoid a) => Monoid (BahEither b a) where
  mempty = PLeft mempty

--

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity 
  (<*>) (Identity f) (Identity a) = Identity $ f a 

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a 

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

--

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor (List) where
  fmap f (Cons a as) = Cons (f a) (fmap f as) 
  fmap _ (Nil) = Nil 

instance Applicative (List) where
  pure a = Cons a Nil 
  (<*>) _ (Nil) = Nil 
  (<*>) (Nil) _ = Nil
  (<*>) (Cons f fs) as = (f <$> as) <> (fs <*> as)

instance Monad (List) where
  return = pure
  (>>=) (Nil) _ = Nil 
  (>>=) (Cons a as) f = (f a) <> (as >>= f) 

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =
    frequency [
      (1, return Nil),
      (1, Cons <$> arbitrary <*> arbitrary)
    ]

instance Monoid (List a) where
  mempty = Nil

instance Semigroup (List a) where
  Nil <> ls = ls
  ls <> Nil = ls
  (Cons x xs) <> ls = Cons x (xs <> ls)

instance Eq a => EqProp (List a) where
  (=-=) = eq

--

j :: Monad m => m (m a) -> m a
j m = m >>= id

--

main = do
  let nopetrig :: Nope (Int, String, Int)
      nopetrig = undefined
  let bahtrig :: BahEither String (Int, String, Int)
      bahtrig = undefined
  let idtrig :: Identity (Int, String, Int)
      idtrig = undefined
  let lstrig :: List (Int, String, Int)
      lstrig = undefined
  quickBatch $ functor nopetrig
  quickBatch $ applicative nopetrig
  quickBatch $ monad nopetrig
  quickBatch $ functor bahtrig
  quickBatch $ applicative bahtrig
  quickBatch $ monad bahtrig
  quickBatch $ functor idtrig
  quickBatch $ applicative idtrig
  quickBatch $ monad idtrig
  quickBatch $ functor lstrig
  quickBatch $ applicative lstrig
  quickBatch $ monad lstrig
