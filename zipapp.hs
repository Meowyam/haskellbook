module ZipApp where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take 3000 l
          ys' = let (ZipList' l) = ys
                in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat x
  (ZipList' y) <*> (ZipList' z) = ZipList' $ go y z
    where
      go [] _ = []
      go _ [] = []
      go (f:fs) (x:xs) = (f x) : (go fs xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

main :: IO ()
main =
  let ls = ZipList' [("b", "w", 1 :: Int)]
  in quickBatch (applicative ls)
