{-# LANGUAGE NoImplicitPrelude #-}

module PrettyReader where

flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

const :: a -> b -> a
const a b = a

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \a -> f (g a)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
  return :: a -> f a
  (>>=) :: f a -> (a -> f b) -> f b

instance Functor ((->) r) where
  fmap = (.)

instance Applicative ((->) r) where
  pure = const
  f <*> a = \r -> f r (a r)

instance Monad ((->) r) where
  return = pure
  m >>= k = flip k <*> m

-- reader t
-- normally see readert
-- it's a monad transformer

withReaderT
  :: (r' -> r)
  -- ^ The function to modify
  -- the environment
  -> ReaderT r m a
  -- ^ Computation to run in the
  -- modified environment
  -> ReaderT r' m a

withReaderT f m =
  ReaderT $ runReaderT m . f





