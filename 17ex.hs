import System.IO
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Type []

one :: a -> [a]
one = pure

oneapp :: [(a -> b)] -> [a] -> [b]
oneapp = (<*>)

-- type IO
two :: a -> IO a
two = pure

twoapp :: IO (a -> b) -> IO a -> IO b
twoapp = (<*>)

-- type (,) a
three :: Monoid a => b -> (a, b)
three = pure

threeapp :: Monoid a => (a, (b -> c)) -> (a, b) -> (a, c)
threeapp = (<*>)

-- type (->) e
four :: a -> (e -> a)
four = pure

fourapp :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
fourapp = (<*>)

-- write instances

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y) 

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Applicative Pair where
  pure x = Pair x x
  Pair x y <*> Pair a b = Pair (x a) (y b)

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

--

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two x y) = Two (a <> x) (b <> y)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two a b)(Two x y) = Two (a <> x)(b <> y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (Two a b) <*> (Two x y) = Two (a <> x) (b y)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

--

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Semigroup a, Semigroup b, Semigroup c)
  => Semigroup (Three a b c) where
  (Three a b c) <> (Three x y z) = Three (a <> x) (b <> y) (c <> z)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend (Three a b c)(Three x y z) = Three (a <> x)(b <> y)(c <> z)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three a b c) <*> (Three x y z) = Three (a <> x) (b <> y) (c z)

--

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

instance (Semigroup a, Semigroup b)
  => Semigroup (Three' a b) where
  (Three' a b c) <> (Three' x y z) = Three' (a <> x) (b <> y) (c <> z)

instance (Monoid a, Monoid b) => Monoid (Three' a b) where
  mempty = Three' mempty mempty mempty
  mappend (Three' a b c)(Three' x y z) = Three' (a <> x)(b <> y)(c <> z)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a b c) <*> (Three' x y z) = Three' (a <> x) (b y) (c z)

--

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
  => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four w x y z) = Four (a <> w) (b <> x) (c <> y) (d <> z)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (Four a b c d) <*> (Four w x y z) = Four (a <> w) (b <> x) (c <> y) (d z)

--

data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d)

instance (Semigroup a, Semigroup b)
  => Semigroup (Four' a b) where
  (Four' a b c d) <> (Four' w x y z) = Four' (a <> w) (b <> x) (c <> y) (d <> z)

instance (Monoid a, Monoid b) => Monoid (Four' a b) where
  mempty = Four' mempty mempty mempty mempty

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' a b c d) <*> (Four' w x y z) = Four' (a <> w) (b <> x) (c <> y) (d z)

--

main :: IO ()
main = do
  quickBatch (applicative (undefined :: Pair (String, String, String)))
  quickBatch (applicative (undefined :: Two String (String, String, String)))
  quickBatch (applicative (undefined :: Three String String (String, String, String)))
  quickBatch (applicative (undefined :: Three' String (String, String, String)))
  quickBatch (applicative (undefined :: Four String String String (String, String, String)))
  quickBatch (applicative (undefined :: Four' String (String, String, String)))
