--1. functions
--not :: Bool -> Bool
--length :: [a] -> Int
--concat :: [[a]] -> [a]
--head :: [a] -> a
--(<) :: Ord a => a -> a-> Bool
--
--type arguments
--if g :: a -> b -> c -> b then type of g 0 'c' "woot" is Char
--if h :: (Num a, Num b) => a -> b -> b then h 1.0 2 is Num b => b
--if h :: (Num a, Num b) => a -> b -> b then h 1 (5.5 :: Double) is Double
--if jackal :: (Ord a, Eq b) => a -> b -> a then jackal "keyboard" "has the word jackal in it" is [Char]
--if jackal :: (Ord a, Eq b) => a -> b -> a then jackal "keyboard" is Eq b => b -> [Char]
--if kessel :: (Ord a, Num b) => a -> b ->a, then kessel 1 2 is a
--if kessel :: (Ord a, Num b) => a -> b -> a, then kessel 1 (2 :: Integer) is (Num a, Ord a) => a
--if kessel :: (Ord a, Num b) => a -> b -> a, then kessel (1 :: Integer) 2 is Integer

--parametricity

p1 :: a -> a
p1 x = x

p2 :: a -> a -> a
p2 a b = a

p2' :: a -> a -> a
p2' a b = b

p3 :: a -> b -> b
p3 a b = b --behaviour doesn't change

--typeinterference
myConcat :: [Char] -> [Char]
myConcat x = x ++ " yo"

--myMult :: Fractional a => a -> a
myMult x = (x/3) * 5

myTake :: Int -> [Char]
myTake x = take x "hey you"

--myCom :: Int -> Bool
myCom x = x > (length [1..10])

-- myAlph :: Char -> Bool
myAlph x = x < 'z'

--chapter exercises
--A value of type [a] is a list of elements that are all of some type a
--A function of type [[a]] -> [a] could take a list of strings as an argument
--a function of type [a] -> Int -> a returns one element of type a from a list
--a function of type (a, b) -> a takes a tuple argument and returns the first value
--
--determine the type

det1a :: Num a => a 
det1a = (*9)6

det1b :: Num a => (a, [Char])
det1b = head[(0,"doge"),(1,"kitteh")]

det1c :: (Integer, [Char])
det1c = head [(0 :: Integer,"doge"),(1,"kitteh")]

det1d :: Bool
det1d = if False then True else False

det1e :: Int
det1e = length [1, 2, 3, 4, 5]

det1f :: Bool 
det1f = (length [1, 2, 3, 4]) > (length "TACOCAT") 

w :: Num a => a
w = y * 10
  where y = x + 5
        x = 5

x = 5
y = x + 5
z :: Num a => a -> a
z y = y * 10

f4 :: Fractional a => a 
f4 = 4 / y
  where x = 5
        y = x + 5

f5 :: [Char]
f5 = x ++ y ++ z
  where x = "Julie"
        y = " <3 "
        z = "Haskell"

-- does it compile?
--
bigNum :: Integer 
bigNum = (^) 5 $ 10
-- wahoo doesn't work because nothing is applied to the 10
wahoo :: Integer
wahoo = (^) bigNum $ 10

-- works but something has to be printed--
comp2x = print
comp2y = print "woohoo!"
comp2z = comp2x "hello world"

comp3a = (+)
comp3b = 5
-- can't apply 5 to 10. possible fix is it's meant to be some kind of tuple
comp3c :: (Integer, Integer)
comp3c = (comp3b, 10)
comp3d :: ((Integer, Integer), Integer)
comp3d = (comp3c, 200)

-- c is not defined
--a = 12 + b
--b = 10000 * c
a :: Num x => x -> x
a c = 12 + b
  where b = 10000 * c

-- type variable or specific type constructor?
--
-- f :: zed [1] -> Zed [2] -> Blah [3] IS
--     fully polymorphic [1] -> concrete [2] -> concrete [3]
-- f :: Enum b [1] => a [2] -> b [3] -> C [4] IS
--      constrained polymorphic [1] -> fully polymorphic [2] -> constrained [3] -> concrete [4]
--f :: f [1] -> g [2] -> C [3] IS
--     fully polymorphic [1] -> fully [2] -> concrete [3]
--
--write a type signature
--
functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y =
  if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

-- given type write function

i :: a -> a
i a = id a

c :: a -> b -> a
c a b = a

-- basically yes. c'' and c are the same.
c'' :: b -> a -> b
c'' a b = a

c' :: a -> b -> b
c' a b = b

r :: [a] -> [a]
r xs = xs
---or
r' :: [a] -> [a]
r' xs = reverse xs
---or
r'' :: [a] -> [a]
r'' xs = tail xs

co :: (b -> c) -> (a -> b) -> a -> c
co x y = x . y

a' :: (a -> c) -> a -> a
a' x y = y

a'' :: (a -> b) -> a -> b
a'' x y = x y

