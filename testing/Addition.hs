module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is\
      \ 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
    it "3 multiplied by 4 is 12" $ do
      multipliedBy 3 4 `shouldBe` 12 
    it "4 multiplied by 5 is 20" $ do
      multipliedBy 4 5 `shouldBe` 20 
    it "x + 1 is always\
      \ greater than x" $ do
    property $ \x -> x + 1 > (x :: Int)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise =
              go (n - d) d (count + 1)

multipliedBy :: (Eq a, Num a) => a -> a -> a
multipliedBy x y = go x y 
  where go x y
          | y == 0 = 0
          | otherwise =
            x + (multipliedBy x (y-1))

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b)
         => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)


genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c)
            => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b)
          => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- What QuickCheck does so
-- you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
          , (3, return (Just a))]


