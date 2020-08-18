module WordNumber where

import Data.List (intersperse)

numberWords = ["zero","one","two","three","four","five","six","seven","eight","nine"]

---get value from index
--single value digit
digitToWord :: Int -> String
digitToWord n = numberWords !! n 

--many value digit
--turn into list of digits
digits :: Int -> [Int]
digits n
  | x < 10= [x]
  | otherwise = digits (div x 10) ++ [mod x 10]
  where x = abs n

wordNumber :: Int -> String
wordNumber n 
  | n < 0 = "negative-" ++ phrase 
  | otherwise = phrase 
  where phrase = concat (intersperse "-" (map digitToWord (digits n)))
