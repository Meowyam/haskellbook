module Print3 where

myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting = 
          concat [hello, " ", world]

syntax1 = (++) [1, 2, 3] [4, 5, 6]
syntax2 = "<3" ++ " Haskell"
syntax3 = concat ["<3", " Haskell"]

print3 = do
  putStrLn greeting
    where greeting = "Yarrrr"

--chapter exercises

synt1a = concat [[1,2,3],[4,5,6]]
synt1b = (++) [1,2,3][4,5,6]
synt1c = (++) "hello" " world"
synt1d = ["hello" ++ " world"]
synt1e = "hello" !! 4
synt1f = (!!) "hello" 4
synt1g = take 4 "lovely"
synt1h = take 3 "awesome"

--2
--[6,12,18]
--rainbow
--10
--Jules
--[2,3,5,6,8,9]

build1a x = x ++ "!"
build1b x = x !! 4
build1c x = drop 9 x

thirdLetter :: [a] -> a
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = s !! x
  where s = "curry is awesome!"

rvrs = drop 9 x ++ drop 5 (take 8 x) ++ " " ++ take 5 x
           where x = "Curry is awesome"
