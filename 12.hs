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

-- small library for maybe

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing (Just a) = False
isNothing Nothing = True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f y = case y of
                    Nothing -> x
                    Just z -> f z

fromMaybe :: a -> Maybe a -> a
fromMaybe x y = mayybee x id y 

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:[]) = Nothing
listToMaybe (a:as) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just n) = n : []

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (a:as) = case a of 
                     Nothing -> [] ++ catMaybes as
                     Just a -> a : catMaybes as

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:_) = Nothing
flipMaybe (Just a:as) = case flipMaybe as of
                     Nothing -> Nothing
                     Just ls -> Just (a:ls)

-- either
--
lefts' :: [Either a b] -> [a]
lefts' = foldr lefty [] where
  lefty (Left x) y =  x : y
  lefty (Right _) y = y

rights' :: [Either a b] -> [b]
rights' = foldr righty [] where
  righty (Left _) y = y
  righty (Right x) y = x : y

partitionEithers' :: [Either a b]
                  -> ([a], [b])
partitionEithers' = foldr party ([],[]) where
  party (Left x) (l, r) = (x:l, r)
  party (Right x) (l, r) = (l, x:r)

eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c

eitherMaybe'' f b = either' (\a -> Nothing) (Just . f) b

-- unfolds

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr f b = case f b of
                  Just (a, b) -> a: myUnfoldr f b
                  Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x

-- tree builder
--
data BinaryTree a =
  Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

-- unfold for binary tree

unfold :: (a -> Maybe (a,b,a))
       -> a
       -> BinaryTree b
unfold f a = case f a of
               Nothing -> Leaf 
               Just (a, b, c) -> Node (unfold f a) b (unfold f c) 

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (goTree n) 0 where
  goTree n x
    | (n > x) = Just (x+1, x, x+1)
    | otherwise = Nothing
