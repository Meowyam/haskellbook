--grab bag

--(mTh x y z = x * y * z) ==
--(mth = \x -> \y -> \z -> x * y * z) ==
--(mth x = \y -> \z -> x * y * z) ==
--(mth x y = \z -> x * y * z)
--
--if type of mTh is Num a => a -> a -> a -> a
--and mTh 3 = \y -> \z -> x * y * z
--the type is Num a => a -> a-> a

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

--addFive x y = (if x > y then y else x) + 5
addFive = \x -> \y -> (if x > y then y else x) + 5

--mflip f = \x -> \y -> f y x
mflip f x y = f y x

-- exercises: variety pack
k :: (a, b) -> a
k (x, y) = x
k1 = k ((4-1), 10)
-- k2 is type String [Char] instead of Integer
k2 :: [Char]
k2 = k ("three", (1 + 2))
k3 = k (3, True)
--k1 and k3 will return 3.

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))

f (a, b, c) (d, e, f) = ((a, d), (c, f))

-- case practice

functionC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- artful dodgy

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

-- guard duty

avgGrade :: (Fractional a, Ord a)
  => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100

pal :: Eq a => [a] -> Bool

pal xs
  | xs == reverse xs = True
  | otherwise = False
-- True when xs is a palindrome
-- pal takes a list

numbers:: (Ord a, Num a, Num b) => a -> b

numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1
-- it gives indication of whether its argument is a positive or negative number or 0
-- numbers takes a Num

fncmp1 = take 5 . enumFrom $ 3

pntfree x = length(filter(== 'a') x)

-- chapter exercises

--a polymorphic function may May resolve to values of different types, depending on inputs.

--if f :: Char -> String
--and g :: String -> String
--g.f :: Char  -> [String]

--(Ord a, Num a) => a -> Bool

-- (a -> b) -> c is a higher-order function

-- f :: a -> a
-- f x = x
-- f True :: Bool

tensDigit :: Integral a => a -> a
--tensDigit x = d
--  where xLast = x `div` 10
--        d     = xLast `mod` 10

tensDigit x 
  | x > 9 = (fst $ x `divMod` 10) `mod` 10
  | otherwise = error "There is no tenths digit!"

hunsD :: Integral a => a -> a
hunsD x
  | x > 99 = (fst $ x `divMod` 100) `mod` 10
  | otherwise = error "There is no hundredths digit!"

--foldBool :: a -> a -> Bool -> a
foldBool x y a =
  case a of
  True -> x
  False -> y

foldBoolguard x y a
  | a = x
  | otherwise = y

--g :: (a -> b) -> (a, c) -> (b, c)
g x (y, z) = ((x y), z)