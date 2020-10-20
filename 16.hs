{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr
--functor is mapping over structure
--functors preserve structure

--class Functor f where
--  fmap :: (a -> b) -> f a -> f b

-- what are the kinds?

--f :: a -> a
-- a :: *

-- f :: a -> b a -> T (b a)
-- b and T :: * -> *

-- f :: c a b -> c b a
-- c :: * -> * -> *

data WhoCares a =
    ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

--instance Functor WhoCares where
--  fmap _ ItDoesnt = ItDoesnt
--  fmap _ WhatThisIsCalled =
--    WhatThisIsCalled
--  fmap f (Matter a) = Matter (f a)

-- doesn't abide by identity law, not a valid Functor instanec
instance Functor WhoCares where
  fmap _ ItDoesnt = WhatThisIsCalled
  fmap f WhatThisIsCalled = ItDoesnt
  fmap f (Matter a) = Matter (f a)

data CountingGood a =
  Heisenberg Int a
  deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (Heisenberg n a) =
    Heisenberg (n) (f a)

--- heavy lifting

a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (*2) (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (fmap read ("123"++)) (fmap show ioi)
    in fmap (*3) changed

--
--instance Functor (Or a) where
--fmap _ (First a) = First a
--fmap f (Second b) = Second (f b)
--
-- chapter exercises
--
data Bool =
  False | True
-- no functor

data BoolAndSomethingElse a =
  False' a | True' a
-- kind * -> *, so yes

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

data BoolAndMaybeSomethingElse a =
  Falsish | Truish a
-- yeah same as above

instance Functor BoolAndMaybeSomethingElse where
  fmap f Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

newtype Mu f = InF { outF :: f (Mu f) }

-- https://stackoverflow.com/questions/39770191/functor-instance-for-newtype-mu-f-inf-outf-f-mu-f
-- "You can't", says stackoverflow

data D =
  D (Array Word Word) Int Int
  -- No D has kind *

data Sum b a =
      First a
    | Second b
  -- switch so functor operates on the First value
instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b 

data Company a c b =
    DeepBlue a c
    | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a =
    L a b a
    | R b a b
    deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b =
      Finance
    | Desk a
    | Bloor b

instance Functor (Quant x) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- commented out for multiple instances of K
--data K a b =
--  K a

--instance Functor (K x) where
--  fmap _ (K a) = K a

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b = 
  K a

instance Functor (Flip K x) where
  fmap f (Flip (K a)) = Flip $ K $ f a

data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst x) where
  fmap f (GoatyConst b) = GoatyConst $ f b

data LiftItOut f a =
  LiftItOut (f a)

instance Functor f =>
  Functor (LiftItOut f) where
    fmap x (LiftItOut f) = LiftItOut $ fmap x f

data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap x (DaWrappa f g) = DaWrappa (fmap x f) (fmap x g)

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap x (IgnoringSomething f g) = IgnoringSomething f $ fmap x g

data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap x (Notorious go ga gt) = Notorious go ga (fmap x gt)
