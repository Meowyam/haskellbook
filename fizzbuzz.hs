import Control.Monad
import Control.Monad.Trans.State
-- difference list,5 which provides an operation that can append in constant time, that is, the time it takes doesn’t grow with the length of the list. Programmers sometimes refer to this as O(1) time, using so-called Big O notation
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzbuzzList :: [Integer] -> DL.DList String 
fizzbuzzList list =
--  execState (mapM_ addResult list) []
  execState (mapM_ addResult list) DL.empty

-- https://stackoverflow.com/questions/27609062/what-is-the-difference-between-mapm-and-mapm-in-haskell
-- "mapM_ is useful for executing something only for its side effects"

--addResult :: Integer -> State [String] ()
addResult :: Integer
          -> State (DL.DList String) ()

addResult n = do
  xs <- get
  let result = fizzBuzz n
  --put (result : xs)
  -- snoc appends to the end, unlike
  -- cons which adds to the front
  put (DL.snoc xs result)

main :: IO ()
main =
  mapM_ putStrLn $ fizzbuzzList [1..100]


