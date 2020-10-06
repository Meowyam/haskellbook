module Main where
import Lib ( caesar, doVigenere, unCaesar, unVigenere )
import Test.QuickCheck

randomChar :: Gen Char
randomChar = elements ['a'..'z']

randomString :: Gen String
randomString = listOf randomChar

genCaesar :: Gen (Int, String)
genCaesar = do
  n <- arbitrary
  message <- randomString
  return (n, message)

genVigenere :: Gen (String, String)
genVigenere = do
  k <- randomChar
  ks <- randomString
  m <- randomChar
  ms <- randomString
  return ((k:ks), (m:ms))

testCaesar :: Property
testCaesar = forAll genCaesar (\(n, message) ->
  unCaesar n (caesar n message) == message)

testVigenere :: Property
testVigenere = forAll genVigenere (\(keyword, message) ->
  unVigenere keyword (doVigenere keyword message) == message)

main :: IO ()
main = do
  quickCheck testCaesar
  quickCheck testVigenere
