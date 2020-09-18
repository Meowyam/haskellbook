module Cipher where
import Data.Char
import Data.List
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

alphabet = ['a'..'z']
countChar c = length $ takeWhile(/=c) alphabet 

--caesar

caesar 0 x = x
caesar n [] = []
caesar n (x:xs) = (alphabet !! (n + (countChar x))) : caesar n xs

unCaesar 0 x = x
unCaesar n [] = []
unCaesar n (x:xs)
  | (countChar x) < n = (alphabet !! ((countChar x) + (length alphabet) - n)) : unCaesar n xs
  | otherwise = (alphabet!!((countChar x)-n)) : unCaesar n xs

--vigenere

--keyword = "ally"
--message = "meet at dawn"

getLetterval = map countChar

keyVal k = getLetterval k

getMsgval = getLetterval . concat . words

-- drop the last index otherwise it adds another list at the end
whereSpaces = init . scanl1 (+) . (map length . words)

-- I used the cycle
vCryptval m k = [if x > (length alphabet) then x-(length alphabet) else x | x <-(zipWith (+) (cycle (keyVal k)) (getMsgval m))]

vCryptchar m k = map (alphabet !!) $ vCryptval m k

-- split into where spaces should go and UNWORD into a string
addSpaces m whichCrypt = unwords $ foldr (\space acc -> [fst $ splitAt space $ head acc] ++ [snd $ splitAt space $ head acc] ++ (tail acc)) [(whichCrypt)] (whereSpaces m) 

doVigenere m k = addSpaces m (vCryptchar m k)

unCryptval m k = [if x < 0 then x+(length alphabet) else x | x <-(zipWith (-) (getMsgval m)(cycle (keyVal k))) ]
unCryptchar m k = map (alphabet !!) $ (unCryptval m k)

unVigenere m k = addSpaces m (unCryptchar m k)

