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
