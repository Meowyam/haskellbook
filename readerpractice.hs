module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' key [] = Nothing
lookup' key ((a, b):as)
  | (key == a) = Just b
  | otherwise = lookup' key as

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup' 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup' 6 $ zip y z

zs :: Maybe Integer
zs = lookup' 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup' n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer
   -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> (z') <*> (z')

--uncurry :: (a -> b -> c) -> (a, b) -> c

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = (&&) <$> (>3) <*> (<8)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' x Nothing = x
fromMaybe' x (Just n) = n

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)  

main :: IO ()

main = do
  print $
    sequenceA [Just 3, Just 2, Just 1]
  print "fold boolean conjunction operator over sequA results applied to: 3"
  print $ foldr (&&) True $ sequA 3
  print "apply sequA to s'"
  print $ sequA $ fromMaybe 0 s'
  print "apply bolt to ys"
  print $ bolt $ fromMaybe 0 ys

--  print $ sequenceA [x, y]
--  print $ sequenceA [xs, ys]
--  print $ summed <$> ((,) <$> xs <*> ys)
--  print $ fmap summed ((,) <$> xs <*> zs)
--  print $ bolt 7
--  print $ fmap bolt z

--  print $ sequenceA [(>3), (<8), even] 7


