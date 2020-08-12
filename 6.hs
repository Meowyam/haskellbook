--Eq instances

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn x') = x == x' 

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x y)
       (Two x' y') = 
       x == x'
    && y == y'

data StringOrInt =
  TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt x') = x == x'
  (==) (TisAString y) (TisAString y') = y == y'
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq x => Eq (Pair x) where
  (==) (Pair x y) 
       (Pair x' y') =
       x == x'
    && y == y'
    && x == y

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y)
       (Tuple x' y') =
       x == x'
    && y == y'

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _ = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye y) (Goodbye y') = y == y'
  (==) _ _ = False
