import Data.Monoid
import Data.Semigroup
import System.IO
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial 

instance Monoid Trivial where
  mempty = Trivial 
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial 

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a =
  (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a =
  (mempty <> a) == a


-- identity a

newtype Identity a =
  Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity a) (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentAssoc =
  Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool

-- data two

data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two x y) = Two (a <> x) (b <> y)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two a b)(Two x y) = Two (a <> x)(b <> y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc =
  Two (Sum Int) (Sum Int) -> Two (Sum Int) (Sum Int) -> Two (Sum Int) (Sum Int) -> Bool

-- boolconj

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True 
  BoolConj False <> BoolConj _ = BoolConj False 
  BoolConj _ <> BoolConj False = BoolConj False 

instance Monoid BoolConj where
  mempty = BoolConj True
--  mappend (BoolConj True) mempty = BoolConj True
  mappend _ (BoolConj False) = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = 
    frequency [ (1, return (BoolConj True))
              , (1, return (BoolConj False)) ]

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

-- booldisj

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj False <> BoolDisj False = BoolDisj False 
    BoolDisj True <> BoolDisj _ = BoolDisj True 
    BoolDisj _ <> BoolDisj True = BoolDisj True 

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj True) _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = 
    frequency [ (1, return (BoolDisj True))
              , (1, return (BoolDisj False)) ]

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- combine a b

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  f <> g = Combine $ \n -> ((unCombine f) n) <> ((unCombine g) n)

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend f g = Combine $ \n -> ((unCombine f) n) <> ((unCombine g) n)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return (Combine { unCombine = f})

instance Eq (Combine a b) where
  (Combine _) == (Combine _) = True

instance Show (Combine a b) where
  show (Combine { unCombine = _}) = "Combine <function>"

type CombAssoc =
  Combine Int (Sum Int) -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Bool

-- comp a

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Eq (Comp a) where
  (Comp _) == (Comp _) = True

instance Show (Comp a) where
  show (Comp _) = "Comp <function>"

instance (Semigroup a) => Semigroup (Comp a) where
  Comp a <> Comp b = Comp (a . b)

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp id 
  mappend (Comp a) (Comp b) = Comp (a . b)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return (Comp { unComp = f})

type CompAssoc =
  Comp (Sum Int) -> Comp (Sum Int) -> Comp (Sum Int) -> Bool

--

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentAssoc)
  quickCheck (monoidLeftIdentity :: Identity (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Identity (Sum Int) -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two (Sum Int) (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Two (Sum Int) (Sum Int) -> Bool)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: CombAssoc)
  quickCheck (monoidLeftIdentity :: Combine Int (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Combine Int (Sum Int) -> Bool)
  quickCheck (semigroupAssoc :: CompAssoc)
  quickCheck (monoidLeftIdentity :: Comp (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Comp (Sum Int) -> Bool)
