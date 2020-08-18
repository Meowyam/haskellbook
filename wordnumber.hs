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

--digits :: Int -> [Int]
--digits n = undefined

--wordNumber :: Int -> String
--wordNumber n = undefined
