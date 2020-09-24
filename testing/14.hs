module WordNumberTest where

import Test.Hspec
import Data.List

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
  
  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]
  
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100
        `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001
        `shouldBe` "nine-zero-zero-one"

-- wordnumber

numberWords :: [String]
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
