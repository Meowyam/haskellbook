import Data.Monoid
import Data.Foldable

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
