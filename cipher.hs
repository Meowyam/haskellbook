module Cipher where
import Data.Char
import Data.List

alphabet = ['a'..'z']
countChar c = length $ takeWhile(/=c) alphabet 

--caesar

caesar 0 x = x
caesar n [] = []
caesar n (x:xs) = (alphabet !! (n + (countChar x))) : caesar n xs

unCaesar 0 x = x
unCaesar n [] = []
unCaesar n (x:xs)
  | (countChar x) < n = (alphabet !! ((countChar x) + (length alphabet) - n)) : unCaesar n xs
  | otherwise = (alphabet!!((countChar x)-n)) : unCaesar n xs

--vigenere

keyword = "ally"
message = "meet at dawn"

getLetterval = map countChar

keyVal = getLetterval keyword

getMsgval = getLetterval . concat . words

-- drop the last index otherwise it adds another list at the end
whereSpaces = init . scanl1 (+) . (map length . words)

-- I used the cycle
vCryptval m = [if x > (length alphabet) then x-(length alphabet) else x | x <-(zipWith (+) (cycle keyVal) (getMsgval m))]

vCryptchar = map (alphabet !!) . vCryptval

-- split into where spaces should go
splitVcrypted m = unwords $ foldr (\space acc -> [fst $ splitAt space $ head acc] ++ [snd $ splitAt space $ head acc] ++ (tail acc)) [(vCryptchar m)] (whereSpaces m) 

