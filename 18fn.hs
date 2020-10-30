import Control.Monad

--(.) :: (b -> c) -> (a -> b) -> a -> c
--(<$>) :: Functor f => (a -> b) -> f a -> f b
--(<*>) :: Applicative f => f (a -> b) -> f a -> f b

j :: Monad m => m (m a) -> m a
j m = m >>= id
-- j (Just 1) = Just 1

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = f <$> m
-- l1 (+1) (Just 1) = Just 2

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb
-- l2 (+) (Just 1) (Just 2) = Just 3

a :: Monad m => m a -> m (a -> b) -> m b
a ma mb = mb <*> ma
-- a (Just 1) (Just +2) = Just 3

meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- meh [1, 2, 3] -> [1 2 3] (\x -> Just (x + 1)) -> [1+1,2+1,3+1]
meh [] f = pure []
meh (x:xs) f = do
  b <- f x
  bs <- meh xs f
  return (b:bs)

flipType :: (Monad m) => [m a] -> m [a]
flipType (x:xs) = meh (x:xs) id
-- flipType [Just 1, Just 2, Just 3] = Just [1, 2, 3] 

