import Data.Int

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
