import Data.List(sort)

i :: Num a => a
i = 1

-- if i :: a and i = 1; gives error no instance for (Num a)
-- haskell infers i must be a Num!!
--

--f :: Float
-- if f :: Num a => a, "Could not deduce (Fractional a)!!!
-- haskell infers f :: Fractional a => a
--f :: Fractional a => a < -- this works
f :: RealFrac a => a -- this works also
f = 1.0

freud :: a -> a -- this is haskell's inferred type
--freud :: Ord a => a -> a
freud x = x

myX = 1 :: Int

sigmund :: Int -> Int
--sigmund :: a -> a   NO because myX is an Int and cannot match a with Int
--however haskell infers sigmund :: p -> Int
-- sigmund :: Num a => a -> a   no couldn't match with type Int
sigmund x = myX

--jung :: Ord a => [a] -> a  --- inferred type
jung :: [Int] -> Int
jung xs = head (sort xs)

--young :: [Char] -> Char
young :: Ord a => [a] -> a -- haskell infers this, as above
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
--signifier :: Ord a => [a] -> a couldn't match type a with type Char
signifier xs = head (mySort xs)


