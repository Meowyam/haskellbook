import Data.Char
-- determine the kinds

-- id :: a -> a
-- the kind of a is *
-- r :: a -> f a
-- a is *, f is * -> *
--
-- string processing

notThe :: String -> Maybe String
notThe x
  | x == "the" = Nothing
  | otherwise = Just x

replaceThe :: String -> String
replaceThe phrase = unwords $ go xs 
  where
    xs = words phrase
    go [] = []
    go (x:xs)
      | notThe x == Nothing = "a" : go xs
      | notThe x == Just x = x : go xs

checkVowel :: Char -> Bool
checkVowel x
  | x `elem` "aeiou" = True
  | otherwise = False

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel phrase = go xs 
  where
    xs = words (map toLower phrase)
    go [] = 0
    go [_] = 0
    go (x:xs)
      | notThe x == Nothing && (checkVowel (head $ head xs)) = 1 + go xs
      | otherwise = go xs

countVowels :: String -> Integer
countVowels phrase = go xs
  where
    xs = map toLower phrase
    go [] = 0
    go (x:xs)
      | checkVowel x = 1 + go xs
      | otherwise = go xs

-- validate the word

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

-- if vowels > not vowels, return Nothing

checkCons :: String -> Integer
checkCons " " = 0
checkCons phrase = (fromIntegral $ length phrase) - countVowels phrase

mkWord :: String -> Maybe Word'
mkWord str
  | countVowels str > checkCons str = Nothing
  | otherwise = Just (Word' str)

-- it's only natural
-- naturals :: whole numbers from 0 to infinity
--  convert natural numbers to integers and integers to naturals
--  integer incl negativ, nat doesn't

data Nat =
  Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | n == 0 = Just Zero
  | otherwise = case integerToNat (n-1) of
                  Just n -> Just (Succ n)
