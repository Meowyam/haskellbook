module Cipher where
import Data.Char

alphabet = ['a'..'z']
countChar c = length $ takeWhile(/=c) alphabet 
caesar 0 x = x
caesar n [] = []
caesar n (x:xs) = (alphabet !! (n + (countChar x))) : caesar n xs

unCaesar 0 x = x
unCaesar n [] = []
unCaesar n (x:xs)
  | (countChar x) < n = (alphabet !! ((countChar x) + (length alphabet) - n)) : unCaesar n xs
  | otherwise = (alphabet!!((countChar x)-n)) : unCaesar n xs
