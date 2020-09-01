import Data.Int
import Data.Char
import Data.List

--Dog types
data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

data DogueDeBordeaux doge =
  DogueDeBordeaux doge

--
--Doggies is a type constructor
--Doggies :: * -> *
--the kind of Doggiest String is Doggies String :: *
--type of Husky 10 is Husky :: Num a -> Doggies a
--type of Husky (10 :: Integer) is Husky (10 :: Integer) :: Doggies Integer
--type of Mastiff "Scooby Doo" is Mastiff "Scooby Doo" :: Doggies [Char]
--DogueDeBordeaux is a type constructor
--type of DogueDeBordeaux is DogueDeBordeaux :: doge -> DogueDeBordeaux doge 
--type of DogueDeBordeaux "doggie!" is DogueDeBordeaux "doggie!" :: DogueDeBordeaux [Char]
--
--data TypeConstructor a =
----DataConstructor a
----deriving (Eq, Show) -----------------Eq==derive equality operations automatically,
-----------------------------------------Show=allows data to be printed to screen as string
--
----Vehicles

data Price =
  Price Integer deriving (Eq, Show)

data Size =
  Size Integer deriving (Eq, Show)

data Manufacturer =
  Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline =
  PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 200)
united = Plane TakeYourChancesUnited (Size 1000)

-- the type of myCar is:
---- myCar :: Vehicle

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar (_) = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane (_) = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manf _) = manf

-- if we use the above on a plane we'll get an error

getLine :: Vehicle -> Airline 
getLine (Plane line _) = line

--cardinality
--
--data PugType = PugData
--PugType is unbounded
--
-- AIrline also seems unbounded
--
-- if Int8 = 256 = 2^8, Int16 = 2^16
--
-- the cardinality of Int is very large
-- it's: -9223372036854775808 to 9223372036854775807, which is 2 ^ 64
-- -- apparently it's the "bit width of the underlying architecture", ie. 64 bits
-- 
-- Integer appears to be unbounded
--
-- For Example:

-- a nullary consctructor
data Example = MakeExample deriving Show
-- type of MakeExample is Example (MakeExample :: Example), type of Example is 
-- Data Constructor not in scope!

-- a unary constructor
data ExampInt = MakeExample' Int deriving Show
--MakeExample' :: Int -> ExampInt

--pity the bool

data BigSmall =
      Big Bool
    | Small Bool
    deriving (Eq, Show)


-- cardinality of BigSmall = 2 (ie. Bool == True+False) + 2 = 4
--
data NumberOrBool =
  Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)
myNumba = Numba (-128)

--(Int8 is 2^8 + 2) == 258
--
--how does your garden grow?

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden =
  Garden Gardener FlowerType
  deriving Show

data Garden' =
  Gardenia' Gardener
    | Daisy' Gardener
    | Rose' Gardener
    | Lilac' Gardener
  deriving (Eq, Show)

-- programmers

data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

data ThereYet =
  There Float Int Bool
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = 
  [ Programmer os lang | os <- allOperatingSystems, lang <- allLanguages ]

-- there are 16 programmers

--exponentiation

data Quantum =
  Yes
  | No
  | Both
  deriving (Eq, Show)


c1 :: Quantum -> Bool
c1 Yes = True
c1 No = True
c1 Both = True

c2 Yes = True
c2 No = True
c2 Both = False

c3 Yes = False
c3 No = True
c3 Both = True

c4 Yes = False
c4 No = False
c4 Both = True

c5 Yes = False
c5 No = False
c5 Both = False

c6 Yes = True
c6 No = False
c6 Both = True

c7 Yes = True
c7 No = False
c7 Both = False

c8 Yes = False 
c8 No = True
c8 Both = False

----

--theQUAD
--
data Quad =
    One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

--eQuad :: Either Quad Quad
--eQuad = 4+4
--
--prodQuad :: (Quad, Quad)
--proQuad = 4*4
--
--funcQuad :: Quad -> Quad
--funcQuad = 4^4
--
--prodTBool :: (Bool, Bool, Bool)
--prodTBool = 2 * 2 * 2
--
--gTwo :: Bool -> Bool -> Bool
--gTwo = 2 ^ 2 ^ 2
--
--5 digit number:
--fTwo :: Bool -> Quad -> Quad
--fTwo = (2*2) ^ (2*2) ^ 2
--
-- binary tree

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a
        => a
        -> BinaryTree a
        -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a
  = Node (insert' b left) a right
  | b > a
  = Node left a (insert' b right)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right) 

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
        1
        (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
        2
        (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup OK!"
  else error "test failed!"

--- binary trees to lists

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right) 

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a] 

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
        2
        (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- all fine!
--
-- write foldr for BinaryTree

foldTree :: (a -> b -> b)
         -> b
         -> BinaryTree a
         -> b

foldTree _ b Leaf = b

foldTree f b (Node left a right) =
  f a (foldTree f (foldTree f b right) left)

-- chapter exercises

data Weekday =
  Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
--
-- Weekday is a type with five data constructors
f :: Weekday -> String
f Friday = "Miller Time"

-- types defined with the data keywords, must begin with a capital letter
--
-- the function g xs = xs !! (length xs -1)
-- gives the last element of xs
--
-- language exercises

capitalizeWord :: String -> String
capitalizeWord n = toUpper (head n) : tail n

capitalizeParagraph :: String -> String
capitalizeParagraph n = go (capitalizeWord n)
  where
    go [] = []
    go ('.':' ':xs) = ". " ++ (go $ capitalizeWord xs)
    go (x:xs) = x : go xs

-- phone exercise

daPhone = [['0','+','_'],
          ['1'],
          ['2','a','b','c'],
          ['3','d','e','f'],
          ['4','g','h','i'],
          ['5','j','k','l'],
          ['6','m','n','o'],
          ['7','p','q','r','s'],
          ['8','t','u','v'],
          ['9','w','x','y','z'],
          ['*','^'],
          ['#','.',',']]

convo :: [String]
convo = ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol OK. Have u ever tasted alcohol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "OK. Do u think I am pretty Lol",
        "Lol ya",
        "Just making sure rofl ur turn"]

-- convert strings to phone friendly format

convertStr [] = []
convertStr (x:xs)
  | isUpper x == True = ['^', (toLower x)] ++ convertStr xs
  | x == '"' = convertStr xs
  | x == ' ' = '_' : convertStr xs
  | otherwise = x : convertStr xs

-- gets which character it is as a grid, eg. [(2,3)] means it's in row 2, 3 presses
charGrid :: Char -> [[Char]] -> [(Char, Int)]
charGrid c xs = grid c xs 0
grid c [] i = []
grid c (x:xs) i = g x ++ grid c xs (i+1) 
  where
    g x = zip ([head (daPhone !! i)])(map (+1) (elemIndices c x))

getPresses [] = []
getPresses (y:ys)
  | length ys < 0 = []
  | otherwise = charGrid y daPhone : getPresses ys

reverseTaps ys = map getPresses (map convertStr ys)

-- get the most frequent element in a list
theCoolest :: Ord a => [a] -> (Int, a)
theCoolest = maximum . map (\x -> (length x, head x)) . group . sort

-- the coolest phrase is "Lol ya"

coolestLtr :: [String] -> Char
coolestLtr = (snd . theCoolest . map toLower . filter (not . flip elem " .,!")) . concat

coolestWord :: [String] -> String
coolestWord = snd. theCoolest . words . concat

--coolest letter is 'a'
--coolest word is 'ur'
--
-- hutton's razor

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add expr1 expr2) = eval expr1 + eval expr2

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add expr1 expr2) = printExpr expr1 ++ "+" ++ printExpr expr2
