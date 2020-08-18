module WordNumber where

import Data.List (intersperse)

strList = ["zero","one","two","three","four","five","six","seven","eight","nine"]

---get value from index
--single value digit
digitToWord :: Int -> String
digitToWord n = strList !! n 

--many value digit
--turn into list of digits
digitList = []
digits n
  | n < 10= [n]
  | (n >= 10) = digits (div n 10) ++ (mod n 10):[]
--digits :: Int -> [Int]
--digits n = undefined

--wordNumber :: Int -> String
--wordNumber n = undefined
