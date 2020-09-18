import Control.Monad
import System.Exit
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  putStrLn "What is your word?"
  line1 <- getLine
  let checkLine = formatLine line1
  case (checkLine == reverse checkLine) of
    True -> putStrLn "It's a palindrome!"
    False -> do putStrLn "Nope!"
                exitSuccess

formatLine :: String -> String
formatLine = filter (`elem` ['a'..'z']) . map toLower

