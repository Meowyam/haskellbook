module PoemLines where
import Data.Bool
import Data.Char

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs

--true is higher than false
eftBool :: Bool -> Bool -> [Bool]
eftBool = eftGen

eftOrd :: Ordering 
      -> Ordering
      -> [Ordering]
eftOrd = eftGen 

eftInt :: Int -> Int -> [Int]
eftInt = eftGen

eftChar :: Char -> Char -> [Char]
eftChar = eftGen

-- so after writing out each one individually, I realised that everything was essentially this:
-- so here is a generalised eft definition
eftGen xs y
  | xs > y = []
  | xs == y = [xs]
  | otherwise = xs : (eftGen (succ xs) y) 

-- thy fearful symmetry
--
myWords [] = []
myWords (' ':xs) = myWords xs
myWords xs = (takeWhile (/=' ') xs) : (myWords (dropWhile (/=' ') xs))

shouldEqual = [ "Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?"]

myLines :: String -> [String]
myLines [] = []
myLines ('\n':xs) = myLines xs
myLines xs = (takeWhile (/='\n') xs) : (myLines (dropWhile (/='\n') xs))

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
            == shouldEqual)

myGeneric s [] = []
myGeneric s (x:xs)
  | s == x = myGeneric s xs
  | otherwise = takeWhile(/=s) (x:xs) : (myGeneric s $ dropWhile (/=s) (x:xs))

-- lists

mySqr = [x^2 | x <- [1..10]]

compr1 = [x | x <- mySqr, rem x 2 == 0]
--get for x divisible by 2
--so [2^2, 4^2, 6^2, 8^2, 10^2]

compr2 = [(x, y) | x <- mySqr,
                   y <- mySqr,
                   x < 50, y > 50]
                   -- so value of x^2 is less than 50, so up to 7^2=49
                   --y starts at y^2 > 50, so 8^2=64
                   --max y is 10^2 so 100
                   --so eg [(1,64), (1,81), (1,100), (4,64)..(49,64), (49,81), (49,100)]

compr3 = take 5 [(x, y) | x <- mySqr,
                          y <- mySqr,
                          x < 50, y > 50]
-- so as above but only take the first 5 values.

acro xs = [x | x <- xs, elem x ['A'..'Z']]

myString xs = [x | x <- xs, elem x "aeiou"]
-- it grabs and returns any of the letters aeiou from the string 
--
mySq = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

myTuple = [(x, y) | x <- mySq,
                    y <- myCube,
                    x < 50, y < 50]

tupleCount = length myTuple

-- spines
-- 1. [x^y | x <- [1..5], y <- [2, undefined]]
-- bottom
-- 2. take 1 $
-- [x^y | x <- [1..5], y <- [2, undefined]]
-- 1
-- 3. sum [1, undefined, 3]
-- bottom
-- 4. length [1, 2, undefined]
-- 3
-- 5. length $ [1, 2, 3] ++ undefined
-- bottom
-- 6. take 1 $ filter even [1, 2, 3, undefined]
-- 2
-- 7. take 1 $ filter even [1, 3, undefined]
-- bottom
-- 8. take 1 $ filter odd [1, 3, undefined]
-- 1
-- 9. take 2 $ filter odd [1, 3, undefined]
-- 1,3
-- 10. take 3 $ filter odd [1, 3, undefined]
-- bottom

--1. [1, 2, 3, 4, 5]
--normal form!
--2. 1 : 2 : 3 : 4 : _
--whnf
--3. enumFromTo 1 10
--neither
--4. length [1, 2, 3, 4, 5]
--neither
--5. sum (enumFromTo 1 10)
--neither
--6. ['a'..'m'] ++ ['n'..'z']
--neither
--7. (_, 'b')
--whnf
--

--transforming lists of values
--take 1 $ map (+1) [undefined, 2, 3]
-- bottom
-- take 1 $ map (+1) [1, undefined, 3]
-- 2
-- take 2 $ map (+1) [1, undefined, 3]
-- no value
itIsMystery xs =
  map (\x -> elem x "aeiou") xs
-- so it will check and tell you if each element in the list is a vowel
--
--map (^2) [1..10] squares each number from 1 to 10
--map minimum [[1..10], [10..20], [20..30]]
----gives the minimum for each list, so [1,10,20] 
--map sum [[1..5], [1..5], [1..5]], so sums all numbers in each list in the list
----so: [[1+2+3+4+5], [same], [same]]

checkThree = map(\x -> bool x (-x) (x==3)) [1..10]

-- filtering

multipleThree = filter (\x -> (rem x 3 == 0)) [1..30]

countMulti3 = length $ filter (\x -> (rem x 3 == 0)) [1..30] 

removeArts = filter (not . (`elem` ["the", "and", "a"])) . words 

-- zipping
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs)(y:ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] [] = []
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' a (x:xs) (y:ys) = (a x y) : (zipWith' a xs ys)

zip'' x y = zipWith' (,) x y 

--chapter exercises

stripCaps [] = []
stripCaps (x:xs) = filter (\x -> (isUpper x)) (x:xs) 

capFirst (x:xs) = (toUpper x) : xs

capAll [] = []
capAll (x:xs) = (toUpper x) : capAll xs 

capHead x = toUpper $ head x

capHead' = toUpper . head
