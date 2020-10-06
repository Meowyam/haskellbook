module Main where
import Lib ( caesar, doVigenere, unCaesar, unVigenere )
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck testCaesar
  quickCheck testVigenere

testCaesar :: Int -> String -> Bool
testCaesar n message = ((Lib.unCaesar n (Lib.caesar n message)) == message)

testVigenere :: String -> String -> Bool
testVigenere keyword message = (Lib.unVigenere keyword (Lib.doVigenere keyword message)) == message