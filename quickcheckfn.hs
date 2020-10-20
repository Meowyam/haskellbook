import System.IO
import Test.QuickCheck

--fmap id      = id
--fmap (p . q) = (fmap p) . (fmap q)
--We can turn those into the following QuickCheck properties:
functorIdentity :: (Functor f, Eq (f a)) =>
                        f a -> Bool
functorIdentity f =
  fmap id f == f

f :: [Int] -> Bool
f x = functorIdentity x

functorCompose :: (Eq (f c), Functor f) =>
                       (a -> b)
                    -> (b -> c)
                    -> f a
                    -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

c = functorCompose (+1) (*2)
li x = c (x :: [Int])


newtype Identity a = Identity a deriving (Eq)
instance Functor (Identity) where
  fmap f (Identity x) = Identity (f x)

data Pair a = Pair a a deriving (Eq)
instance Functor (Pair) where
  fmap f (Pair x y) = Pair (f x) (f y) 

data Two a b = Two a b deriving (Eq)
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

data Three a b c = Three a b c deriving (Eq)
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z) 

data Three' a b = Three' a b b deriving (Eq)
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z) 

data Four a b c d = Four a b c d deriving (Eq)
instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

data Four' a b = Four' a a a b deriving (Eq)
instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

data Trivial = Trivial
-- no. Trivial only has * kind


main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (Identity (x :: Int)) 
  quickCheck $ \x y -> functorIdentity (Pair (x :: Int) (y :: Int)) 
  quickCheck $ \x y -> functorIdentity (Two (x :: Int) (y :: Int)) 
  quickCheck $ \x y z -> functorIdentity (Three (x :: Int) (y :: Int) (z :: Int)) 
  quickCheck $ \x y -> functorIdentity (Three' (x :: Int) (y :: Int) (y :: Int)) 
  quickCheck $ \w x y z -> functorIdentity (Four (w :: Int) (x :: Int) (y :: Int) (z :: Int)) 
  quickCheck $ \x y -> functorIdentity (Four (x :: Int) (x :: Int) (x :: Int) (y :: Int)) 
