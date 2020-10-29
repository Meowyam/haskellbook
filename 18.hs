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

main = do
  let nopetrig :: Nope (Int, String, Int)
      nopetrig = undefined
  let bahtrig :: BahEither String (Int, String, Int)
      bahtrig = undefined
  quickBatch $ functor nopetrig
  quickBatch $ applicative nopetrig
  quickBatch $ monad nopetrig
  quickBatch $ functor bahtrig
  quickBatch $ applicative bahtrig
  quickBatch $ monad bahtrig
