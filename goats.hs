{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n 

instance TooMany (Int, Int) where
  tooMany (x, y) = tooMany (x + y)

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (num, many) = tooMany (num * many)

newtype Goats =
  Goats Int deriving (Eq, Show)

newtype Cows =
  Cows Int deriving (Eq, Show)

instance TooMany Goats where
  tooMany (Goats n) = tooMany n

