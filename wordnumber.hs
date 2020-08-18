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
  | (abs n) < 10= [(abs n)]
  | otherwise = digits (div (abs n) 10) ++ [mod (abs n) 10]

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))
