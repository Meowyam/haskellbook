f :: Bool -> Maybe Int
f False = Just 0
f _ = Nothing

-- fibonacci numbers

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
--fn calls itself
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

--review of types
--type of [[True, False], [True, True], [False, True]] is [[Bool]]
--and [[3 == 3], [6 > 5], [3 < 4]] has the same type
--
--if func :: [a] -> [a] -> [a]
--func x y = x ++ y
--they must both be lists of the same type, and if one is a string the other must be too
--eg. func "Hello" "World"


cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"
-- missing y
--
frappe :: String -> String
frappe = flippy "haha"

--appedCatty "woohoo" is woops mrow woohoo
--frappe "1" is 1 mrow haha
--frappe (appedCatty "2")
----is frappe (woops mrow 2)
----is (woops mrow 2) mrow haha
----is woops mrow2 mrow haha
--appedCatty (frappe "blue")
----is appedCatty (blue mrow haha)
----is cattyConny woops (blue mrow haha)
----is woops mrow blue mrow haha
--cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
----is cattyConny (pink mrow haha) (cattyConny "green" (cattyConny woops blue))
----is cattyConny (pink mrow haha) (cattyConny green (woops mrow blue))
----is cattyConny (pink mrow haha) (green mrow woops mrow blue)
----is pink mrow haha mrow green mrow woops mrow blue
--cattyConny (flippy "Pugs" "are") "awesome"
----is cattyConny (are mrow Pugs) awesome
----is are mrow Pugs mrow awesome
--

--dividedBy 15 2 ==
--15 - 2, 13  (1)
--13 - 2, 11  (2)
--11 - 2, 9   (3)
--9 - 2, 7    (4)
--7 - 2, 5    (5)
--5 - 2, 3    (6)
--3 - 2, 1    (7)
--so the answer is 7 with remainder of 1

recurseSum n = go n
  where go n 
          | n == 1 = n 
          | otherwise =
            n + recurseSum (n-1)

-- so multiplying eg. 2 * 3 is
-- 2 + 2 + 2
-- and multiplying 3 * 2 is
-- 3 + 3
-- so I'm adding num1 to itself num2 times
--
--2 (3x) +
--2 (3-1 x) +
--2 (2-1 x) +
--2 (0 x)
multInt :: Integral a => a -> a -> a
multInt x y = go x y 
  where go x y
          | y == 0 = 0
          | otherwise =
            x + (multInt x (y-1))


-- -10 + 2 =
-- -8 + 2 =
-- - 6 + 2 =
-- - 4 + 2 =
-- - 2 + 2 = 0
--
data DividedResult =
     Result (Integer, Integer)
   | DividedByZero
   deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num 0 = DividedByZero
dividedBy num denom
  | ((num < 0 && denom > 0) || (denom < 0 && num > 0)) = Result ((negate (fst absSoln)), snd absSoln)
  | otherwise = Result ((fst absSoln), snd absSoln)
  where go n d count
          | n < d = (count, n) 
          | otherwise =
            go (n-d) d (count + 1)
        absSoln = go (abs num) (abs denom) 0
