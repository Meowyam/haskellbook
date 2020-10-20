data Possibly a =
      LolNope
    | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)


--

data Sum a b =
      First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum e) where
  fmap _ (First a) = First a
  fmap f (Second a) = Second (f a)
