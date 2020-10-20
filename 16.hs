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

