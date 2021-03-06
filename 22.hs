{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)
  --same as bbop $ duwop

newtype Reader r a =
  Reader { runReader :: r -> a }

-- type signature in instance declaration is illegal, had to add instancesigs on top
instance Functor (Reader r) where
  fmap :: (a -> b)
       -> Reader r a
       -> Reader r b
  fmap f (Reader ra) =
    Reader $ (f . ra)

ask :: Reader a a
ask = Reader $ \a -> a

--

--pure :: a -> f a
--pure :: a -> (r -> a)

--(<*>) :: f (a -> b)
--      -> f a
--      -> f b

--(*) :: (r -> a -> b)
--    -> (r -> a)
--    -> (r -> b)
--
newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris =
  Person (HumanName "Chris Allen")
         (DogName "Papu")
         (Address "Austin")

getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' =
  liftA2 Dog dogName address

myLiftA2 :: Applicative f =>
            (a -> b -> c)
          -> f a -> f b -> f c

myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks f = f <$> (Reader $ \a -> a)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a 

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r $ ra r

--

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

--barOne :: Foldable t => t a -> (t a, Int)
barOne :: (Functor t, Num a, Foldable t) => t a -> (t a, Int)
barOne r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

fooBind m k = \r -> k (m r) r
fooBind :: (r -> a)
        -> (a -> r -> b)
        -> (r -> b)

-- exercise: reader monad

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r

--newtype Reader r a =
  --Reader { runReader :: r -> a }

getDogRM :: Person -> Dog
getDogRM =
  dogName >>= \name ->
  address >>= \ addy ->
  return $ Dog name addy

