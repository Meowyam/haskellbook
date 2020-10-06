module Lib where
import Data.Char
import Data.List ()

--caesar

shiftChar :: Int -> Char -> Char
shiftChar n x = chr (mod (ord (toLower x) - ord 'a' + n) 26 + ord 'a')

vShift :: Char -> Int
vShift k
  | isLetter k = ord (toLower k) - ord 'a'
  | otherwise = 0

caesar :: Int -> String -> String
caesar n = map (shiftChar n)

unCaesar :: Int -> String -> String
unCaesar n = caesar (-n)
  
--vigenere

--keyword = "ally"
--message = "meet at dawn"
doVigenere :: String -> String -> String
doVigenere ks ms = zipWith go (cycle ks) ms
  where go k m = shiftChar (vShift k) m

unVigenere :: String -> String -> String
unVigenere ks ms = zipWith go (cycle ks) ms
  where go k m = shiftChar (-vShift k) m