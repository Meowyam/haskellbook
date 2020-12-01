import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzbuzzFromTo :: Integer
               -> Integer
               -> [String]
fizzbuzzFromTo from to
  | from > to = []
  | otherwise = fizzBuzz from : (fizzbuzzFromTo (from + 1) to)

main :: IO ()
main =
  mapM_ putStrLn $ fizzbuzzFromTo 1 100

