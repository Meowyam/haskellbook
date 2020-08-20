module Cipher where
import Data.Char

alphabet = ['a'..'z']
countChar c = length $ takeWhile(/=c) alphabet 
caesar 0 (x:xs) = x:xs
caesar n [] = []
caesar n (x:xs) = (alphabet !! (3 + (countChar x))) : caesar n xs


