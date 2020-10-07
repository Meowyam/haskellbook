-- the runtime representation of newtype and what it wraps are always identical
import Data.Monoid

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a
    => Monoid (Optional a) where
  mempty = Nada


instance Semigroup a
      => Semigroup (Optional a) where
  (Nada) <> (Nada) = Nada
  (Nada) <> (Only a) = Only a
  (Only a) <> (Nada) = Only a
  )Only a) (Only b) = Only (a <> b)