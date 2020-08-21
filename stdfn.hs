myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
  | x == True = True
  | otherwise = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny a [] = False
myAny a (x:xs)
  | a x == True = True
  | otherwise = myAny a xs

myElem :: Eq a => a -> [a] -> Bool
myElem a xs 
  | (any (a==) xs) == True = True
  | otherwise = False

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = myReverse (tail xs) ++ [head xs]   

squish :: [[a]] -> [a]
squish [] = []
squish xs = head xs ++ (squish $ tail xs)

squishMap :: (a->[b]) -> [a] -> [b]
squishMap f [] = []
squishMap f xs = f (head xs) ++ squishMap f (tail xs)

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap concat [xs]

myMaximumBy f [] = error "Empty array"
myMaximumBy f (x:[]) = x
myMaximumBy f (x:xs)
  | ((f x go) == LT) = go 
  | otherwise = x
  where go = myMaximumBy f xs


myMinimumBy f [] = error "Empty array"
myMinimumBy f (x:[]) = x
myMinimumBy f (x:xs)
    | ((f x go) == LT) = x
    | otherwise = go
  where go = myMinimumBy f xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
