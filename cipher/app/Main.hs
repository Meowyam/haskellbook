module Main where

import Lib
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "If you want to Caesar press 1, UnCaesar press 2, Vigenere press 3, or UnVigenere press 4."
  getCipher <- getLine
  let whichCipher = read getCipher :: Int
  putStr "Your word: "
  word <- getLine
  putStr "Your key: "
  -- gets key as string
  key <- getLine
  case whichCipher of
    1 -> putStrLn (caesar (read key :: Int) word)
    2 -> putStrLn (unCaesar (read key :: Int) word)
    3 -> putStrLn (doVigenere word key)
    4 -> putStrLn (unVigenere word key)
