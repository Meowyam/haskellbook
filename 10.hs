import Data.Time
-- understanding folds
--
--foldr (*) 1 [1..5] is the same as foldl (flip (*)) 1 [1..5], and foldl (*) 1 [1..5]
--
--foldl (flip (*)) 1 [1..3]
--f = (flip (*))
--( ( (1 f 1) f 2) f 3)
--( ((1) f 2) f 3)
--((2*1) f 3)
--(3*2)
--(6)
--
--foldr associates to the right
--
--folds are catamorphisms which means they reduce structure
--
f5a = foldr (++) [] ["woot", "WOOT", "woot"] 
f5b = foldr max 'a' "fear is the little death"
f5c = foldr (&&) True [False, True]
f5d = foldr (||) False [False, True] -- I guess if you turn True to False it's different
f5e = foldr ((++) . show) "" [1..5]
f5f = foldr ((const) . show) "a" [1..5]
f5g = foldl const 0 "tacos"
f5h = foldr (flip const) 0 "burritos"
f5i = foldr (flip const) 'z' [1..5]

-- database processing

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 111 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 1000 --added to test qns 4 and 5
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
--
--this is the thing filterDbDate filters for in the string
dateItem (DbDate x) xs = x:xs
dateItem _ xs = xs

filterDbDate = foldr dateItem []

filterDbNumber :: [DatabaseItem] -> [Integer]

numberItem (DbNumber x) xs = x:xs
numberItem _ xs = xs

filterDbNumber = foldr numberItem []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
--okay first turn the sum FROM INTEGRAL to double
getNum :: [DatabaseItem] -> Double
getNum = fromIntegral . sumDb
--then getLength as double
getLength :: [DatabaseItem] -> Double
getLength = fromIntegral . length . filterDbNumber

avgDb xs = (getNum xs) / (getLength xs)

--scans

fibs = 1 : scanl (+) 1 fibs

takeFirst20 = take 20 fibs 

takeLess100 = takeWhile (< 100) fibs

--factorial is 3! = 3*2*1 etc.
getFactorial x = (scanl (flip (*)) (1) [1..(x)])

--chapter exercises

stops = "pbtdkg"
vowels = "aeiou"
nouns = ["cat", "bear"]
verbs = ["pet", "lick"]

stopVowelStop = [(stop1, vowel, stop2) | stop1 <- stops, vowel <- vowels, stop2 <- stops] 
pVowelStop = [(stop1, vowel, stop2) | stop1 <- stops, vowel <- vowels, stop2 <- stops, stop1 == 'p'] 
nounVerbNoun = [(n1, v, n2) | n1 <- nouns, v <- verbs, n2 <- nouns] 

-- what is seekrit
--seekritFunc gets the average of the sum of the length of each of the letters of each words in x
--okay so it gets the average number of letters in each word in x
seekritFunc x = div (sum (map length (words x))) (length (words x))

seekritFrac :: String -> Float
seekritFrac x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

-- rewriting functions using folds
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
  | x == True = True
  | otherwise = myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr
        (\a b ->
          if a == True
           then True
           else b) False

myOr'' :: [Bool] -> Bool
myOr'' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny a [] = False
myAny a (x:xs)
  | a x == True = True
  | otherwise = myAny a xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' a = foldr (||) False . map a

myElem :: Eq a => a -> [a] -> Bool
myElem a xs 
  | (any (a==) xs) == True = True
  | otherwise = False

myElem' a = (any (a==))

myElem'' a = foldr (\x y -> y || (x == a) ) False 

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = (f x) : myMap f xs

myMap' f xs = foldr (\a b -> (f a) : b) [] xs

myMap'' f = foldr ((:) . f) []

--eg. filter (>3) [1,2,3,4,5] gives [4,5]
myFilter :: (a -> Bool) -> [a] -> [a]

myFilter f [] = []
myFilter f (x:xs)
    | f x = x : (myFilter f xs) 
    | otherwise = myFilter f xs

myFilter' f xs = foldr (\a b -> if (f a) then a : b else b) [] xs

myFilter'' :: (a -> Bool) -> [a] -> [a]
myFilter'' f = foldr (\a b -> if (f a) then a : b else b) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\a -> a)

myMaximumBy f [] = error "Empty array"
myMaximumBy f (x:[]) = x
myMaximumBy f (x:xs)
  | ((f x go) == LT) = go 
  | otherwise = x
  where go = myMaximumBy f xs

myMaximumBy' :: (a -> a -> Ordering)
             -> [a]
             -> a

myMaximumBy' f (x:xs) = foldr (\a b -> if (f a b == GT) then a else b) x xs

myMinimumBy f [] = error "Empty array"
myMinimumBy f (x:[]) = x
myMinimumBy f (x:xs)
    | ((f x go) == LT) = x
    | otherwise = go
  where go = myMinimumBy f xs

myMinimumBy' :: (a -> a -> Ordering)
             ->[a]
             ->a

myMinimumBy' f (x:xs) = foldr (\a b -> if (f a b == LT) then a else b) x xs
