import Data.List (elemIndex)
import Test.Hspec
import Test.QuickCheck

-- all applicatives are functors
-- fmap  ::   (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b
--
-- ($) :: (a -> b) -> a -> b
-- (<$>) :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b
--
-- There are a few situations to use pure.

-- As freestyle's answer illustrated, when you don't know what specific Applicative is being used.

-- Also from freestyle, when its just more convenient to use pure.

-- In addition to just being simpler to write in some cases, it can make your code more robust. As I've develop software, my key data structures often evolve. Using pure instead of a specific constructor helps create spots of code that can remain unchanged during refactoring.

-- When the implementation of the specific Applicative is unavailable.
-- https://stackoverflow.com/questions/51512233/what-is-the-purpose-of-pure-in-applicative-functor


added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

--

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

--

x' :: Maybe Int
x' = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

--

xs = [1, 2, 3]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ( (,) <$> x'' <*> y'')

-- identity instance

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x)  = f <$> (Identity x)

-- constant
-- you use this when whatever you want to do involves throwing away a function application

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

instance Monoid a
      => Applicative (Constant a) where
  pure f = Constant mempty
  (Constant x) <*> (Constant y) = Constant $ x <> y

--

main :: IO ()
main =
  hspec $ do
    describe "instance applicative identity test" $ do
      it "(+1) <$> Identity 1 `shouldBe` Identity 2" $ do
        (+1) <$> Identity 1 `shouldBe` Identity 2
    describe "constant applicative identity test" $ do
      it "(+1) <$> Constant 1 `shouldBe` Constant 1" $ do
        (+1) <$> Constant 1 `shouldBe` Constant 1
