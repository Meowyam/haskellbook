module UsingQC where

import Test.QuickCheck
import Data.List (sort)
import Data.Char

-- for a function
half :: Fractional a => a -> a
half x = x / 2

-- this property should hold
halfIdentity :: Double -> Double
halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = halfIdentity x == x

-- for any list you apply sort to,
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: Ord a => [a] -> Bool
prop_listOrdered xs = listOrdered $ sort xs

plusAssociative x y z =
  x + (y + z) == (x + y) + z
plusCommutative x y =
  x + y == y + x
multiplyAssoc x y z =
  x * (y * z) == (x * y) * z
multiplyComm x y =
  (x * y) == (y * x)

prop_plusAssociative :: Integral a => a -> a -> a -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative :: Integral a => a -> a -> Bool
prop_plusCommutative x y = x + y == y + x

prop_multiplyAssoc :: Integral a => a -> a -> a -> Bool
prop_multiplyAssoc x y z = x * (y * z) == (x * y) * z

prop_multiplyComm :: Integral a => a -> a -> Bool
prop_multiplyComm x y = x * y == y * x

-- quot rem
--(quot x y) * y + (rem x y) == x
--(div x y) * y + (mod x y) == x
prop_quotRem :: Integral a => a -> a -> Bool
prop_quotRem _ 0 = True
prop_quotRem x y = (quot x y) * y + (rem x y) == x

prop_divMod :: Integral a => a -> a -> Bool
prop_divMod _ 0 = True
prop_divMod x y = (div x y) * y + (mod x y) == x 

prop_powerAssoc :: (Integral b1, Integral b2, Num a, Eq a) => a -> b2 -> b1 -> Bool
prop_powerAssoc x y z = (x ^ y) ^ z == x ^ (y ^ z)
-- failed!!

prop_powerComm :: Integral b => b -> b -> Bool
prop_powerComm x y = x ^ y == y ^ x
-- failed also!!

prop_reverseList :: Eq a => [a] -> Bool
prop_reverseList xs = (reverse . reverse) xs == id xs

--f $ a = f a
--f . g = \x -> f (g x)

prop_dollar :: (Eq a) => (t -> a) -> t -> Bool
prop_dollar f a = (f $ a) == f a

prop_compose :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
prop_compose f g x = (f . g $ x) == f (g x)

prop_foldrcolon :: Eq a => [a] -> [a] -> Bool
prop_foldrcolon xs ys = foldr (:) xs ys == (++) xs ys
-- failed!

prop_foldrconcat :: (Eq a, Foldable t) => t [a] -> Bool
prop_foldrconcat xs = foldr (++) [] xs == concat xs

prop_lengthtake :: Int -> [a] -> Bool
prop_lengthtake n xs = length (take n xs) == n
-- failed!

prop_readshow :: (Eq a, Read a, Show a) => a -> Bool
prop_readshow x = (read (show x)) == x

-- Failure! Why does it fail!

square :: Num a => a -> a
square x = x * x

squareIdentity :: Float -> Float
squareIdentity = square . sqrt

prop_squareIdentity :: Float -> Bool
prop_squareIdentity x = squareIdentity x == x
-- Failure!
-- The IEEE-754 Standard for Binary Floating-Point Arithmetic [1] requires that the result of a divide or square root operation be calculated as if in infinite precision
-- so sqrt has type of Float

-- idempotence
--
twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

capitalizeWord :: [Char] -> [Char]
capitalizeWord = map toUpper

prop_capitalizeWord :: [Char] -> Bool
prop_capitalizeWord x =
  (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

prop_sort :: Ord a => [a] -> Bool
prop_sort x =
  (sort x == twice sort x) && (sort x == fourTimes sort x)

-- make a Gen random generator

-- equal probabilities
data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary = elements [Fulse, Frue]

-- 2/3 fulse, 1/3 frue
data Fool' =
    Fulse'
  | Frue'
  deriving (Eq, Show)

instance Arbitrary Fool' where
  arbitrary = frequency [(2, return $ Fulse'), (1, return $ Frue')]

run :: IO ()
run = do
  quickCheck (prop_halfIdentity :: Double -> Bool)
  quickCheck (prop_listOrdered :: [Double] -> Bool)
  quickCheck (prop_plusAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (prop_plusCommutative :: Integer -> Integer -> Bool)
  quickCheck (prop_multiplyAssoc :: Integer -> Integer -> Integer -> Bool)
  quickCheck (prop_multiplyComm :: Integer -> Integer -> Bool)
  quickCheck (prop_quotRem :: Integer -> Integer -> Bool)
  quickCheck (prop_divMod :: Integer -> Integer -> Bool)
  quickCheck (prop_powerAssoc :: Integer -> Integer -> Integer -> Bool)
  quickCheck (prop_powerComm :: Integer -> Integer -> Bool)
  quickCheck (prop_reverseList :: [Int] -> Bool)
  quickCheck (prop_dollar (+1) :: Int -> Bool)
  quickCheck (prop_compose (+1) (+2) :: Int -> Bool)
  quickCheck (prop_foldrcolon :: [Int] -> [Int] -> Bool)
  quickCheck (prop_foldrconcat :: [[Int]] -> Bool)
  quickCheck (prop_lengthtake :: Int -> [Int] -> Bool)
  quickCheck (prop_readshow :: String -> Bool)
  quickCheck (prop_squareIdentity :: Float -> Bool)
  quickCheck (prop_capitalizeWord :: String -> Bool)
  quickCheck (prop_sort :: [Int] -> Bool)
