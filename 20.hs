import Data.Monoid
import Data.Foldable

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--

data Identity a =
  Identity a
  deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional a =
  Nada 
  | Yep a
  deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z
  foldl _ z Nada = z
  foldl f z (Yep x) = f z x
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

-- we need to assert a type that has a Monoid for this to work
-- foldMap (+1) Nada :: Sum Int
-- Sum {getSum = 0} -- Sum 0 is mempty for Sum
-- foldMap (+1) Nada :: Product Int
-- {getProduct = 1} -- Product 1 is mempty for Product

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . (foldMap Sum)
--sum' [1,2,3] = 6

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . (foldMap Product)
--product' [2,3] = 6

elem' :: (Foldable t, Eq a)
     => a -> t a -> Bool
elem' a = getAny . (foldMap $ fmap Any (== a))
--elem' 3 [1,2,3] = True
--elem' 3 [1,2] = False

minimum' :: (Foldable t, Ord a)
         => t a -> Maybe a
minimum' = foldr getMin Nothing
  where
    getMin x Nothing = Just x
    getMin x (Just y) = Just (min x y)

maximum' :: (Foldable t, Ord a)
         => t a -> Maybe a
maximum' = foldr getMax Nothing
  where
    getMax x Nothing = Just x
    getMax x (Just y) = Just (max x y)

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ len -> len + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x xs -> x : xs) []

--toList' (Just 1) = [1]

--foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (mappend mempty)
--fold' [Sum 1, Sum 2]
--Sum {getSum = 3} 

foldMap'' :: (Foldable t, Monoid m)
          => (a -> m) -> t a -> m
foldMap'' f = foldr (\x -> ((<>) (f x))) mempty
-- foldMap'' Sum [1,2,3] = 6
--
-- write foldable instances
--
data Constant a b =
  Constant b
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant x) = f x

instance Arbitrary b => Arbitrary (Constant a b) where
  arbitrary = do
    b <- arbitrary
    return $ Constant b

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

--

data Two a b =
  Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two x y) = f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

--

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

--

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

--

data Four' a b =
  Four' a b b b
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' w x y z) = f x 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

--
filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
           => (a -> Bool) -> t a -> f a
filterF f = foldr (\x y ->
  if f x
     then (pure x) <> y
     else y)
  mempty
--

type Testblob = (Int, String, [Int], Int, String)

main = do
  let consttrig :: Constant Int Testblob 
      consttrig = undefined
  let twotrig :: Two Int Testblob
      twotrig = undefined
  let threetrig :: Three Int Int Testblob
      threetrig = undefined
  let thretrig :: Three' Int Testblob
      thretrig = undefined
  let fourtrig :: Four' Int Testblob
      fourtrig = undefined
  quickBatch $ foldable consttrig
  quickBatch $ foldable twotrig
  quickBatch $ foldable threetrig
  quickBatch $ foldable thretrig
  quickBatch $ foldable fourtrig
