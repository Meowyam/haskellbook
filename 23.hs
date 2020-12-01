{-# LANGUAGE InstanceSigs #-}

newtype Moi s a =
  Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b)
       -> Moi s a
       -> Moi s b
  fmap f (Moi g) =
    Moi $ \s -> (f $ fst (g s), snd (g s))

--newtype Reader r a =
  --Reader { runReader :: r -> a }

-- type signature in instance declaration is illegal, had to add instancesigs on top
--instance Functor (Reader r) where
--  fmap :: (a -> b)
--       -> Reader r a
--       -> Reader r b
--  fmap f (Reader ra) =
--    Reader $ (f . ra)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi $ \s -> (,) <$> (fst $ f s) . fst <*> snd $ g s

--instance Applicative (Reader r) where
--  pure :: a -> Reader r a
--  pure a = Reader $ const a 

--  (<*>) :: Reader r (a -> b)
--        -> Reader r a
--        -> Reader r b
--  (Reader rab) <*> (Reader ra) =
--    Reader $ \r -> rab r $ ra r

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g = 
    Moi $ \s -> runMoi (g . fst $ f s) s

--instance Monad (Reader r) where
--  return = pure

----  (>>=) :: Reader r a
  ----      -> (a -> Reader r b)
  ----      -> Reader r b
----  (Reader ra) >>= aRb =
 ----   Reader $ \r -> runReader (aRb (ra r)) r

-- chapter exercises

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s = Moi $ \s -> ((), s)

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
