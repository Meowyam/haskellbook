module Lib where
import Data.Char
import Data.List ()

alphabet :: [Char]
alphabet = ['a'..'z']
countChar :: Char -> Int
countChar c = length $ takeWhile(/=c) alphabet 

--caesar

caesar :: Int -> String -> String
caesar 0 x = x
caesar n [] = []
caesar n (x:xs) = (alphabet !! (n + (countChar x))) : caesar n xs
  where x = toLower x

unCaesar :: Int -> String -> String
unCaesar 0 x = x
unCaesar n [] = []
unCaesar n (x:xs)
  | (countChar x) < n = (alphabet !! ((countChar x) + (length alphabet) - n)) : unCaesar n xs
  | otherwise = (alphabet!!((countChar x)-n)) : unCaesar n xs
    where x = toLower x

--vigenere

--keyword = "ally"
--message = "meet at dawn"

getLetterval :: String -> [Int]
getLetterval = map countChar

keyVal :: String -> [Int]
keyVal k = getLetterval k

getMsgval :: String -> [Int]
getMsgval = getLetterval . concat . words

-- drop the last index otherwise it adds another list at the end
whereSpaces :: String -> [Int]
whereSpaces = init . scanl1 (+) . (map length . words)

-- I used the cycle
vCryptval :: String -> String -> [Int]
vCryptval m k = [if x > (length alphabet) then x-(length alphabet) else x | x <-(zipWith (+) (cycle (keyVal k)) (getMsgval m))]

vCryptchar :: String -> String -> String
vCryptchar m k = map (alphabet !!) $ vCryptval m k

-- split into where spaces should go and UNWORD into a string
addSpaces :: String -> String -> String
addSpaces m whichCrypt = unwords $ foldr (\space acc -> [fst $ splitAt space $ head acc] ++ [snd $ splitAt space $ head acc] ++ (tail acc)) [(whichCrypt)] (whereSpaces m) 

doVigenere :: String -> String -> String
doVigenere m k = addSpaces m (vCryptchar m k)

unCryptval :: String -> String -> [Int]
unCryptval m k = [if x < 0 then x+(length alphabet) else x | x <-(zipWith (-) (getMsgval m)(cycle (keyVal k))) ]
unCryptchar :: String -> String -> String
unCryptchar m k = map (alphabet !!) $ (unCryptval m k)

unVigenere :: String -> String -> String
unVigenere m k = addSpaces m (unCryptchar m k)


