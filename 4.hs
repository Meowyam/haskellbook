import Data.Tuple

data Mood = Blah | Woot deriving Show
--it's a bool of blah or woot
-- woot isn't a type, it still returns type mood
changeMood :: Mood -> Mood 
changeMood Blah = Woot
changeMood Woot = Blah

--find the mistakes

--not True && True
--not x = 6
--(1*2) > 5
--["Merry"]>["Happy"]
--show [1,2,3] ++ "look at me!"

-- chapter exercises

-- length would give Int and takes one argument of a list
-- length :: [a] -> Int
-- 5, 3, 2 (two lists), 5
-- 6 / length[1,2,3] returns an error.
qn4 = 6 `div` length[1,2,3] 
-- True,Bool
-- num; bool, false
-- 7a: true,
-- b: no, all elements in list need to be same type,
-- c: 5
-- d: false
-- e: doesn't owrk because 9 isn't bool

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (reverse x == x) == True

myAbs :: Integer -> Integer
myAbs x = if x < 0 
            then negate x
          else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+)
addOne xs = w `x` 1
  where w = length xs

id :: x -> x
id x = x

first :: (a, b) -> a
first (a, b) = fst (a, b)

--match the function names to their types
--c  show is type Show a => a -> String
--b  == is type Eq a => a -> a -> Bool
--c  fst is type (a, b) -> a
--d  + is type Num a => a -> a -> a
