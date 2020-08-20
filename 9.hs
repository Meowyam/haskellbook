module PoemLines where
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen


safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs

--true is higher than false
eftBool :: Bool -> Bool -> [Bool]
eftBool = eftGen

eftOrd :: Ordering 
      -> Ordering
      -> [Ordering]
eftOrd = eftGen 

eftInt :: Int -> Int -> [Int]
eftInt = eftGen

eftChar :: Char -> Char -> [Char]
eftChar = eftGen

-- so after writing out each one individually, I realised that everything was essentially this:
-- so here is a generalised eft definition
eftGen xs y
  | xs > y = []
  | xs == y = [xs]
  | otherwise = xs : (eftGen (succ xs) y) 

-- thy fearful symmetry
--
myWords [] = []
myWords (' ':xs) = myWords xs
myWords xs = (takeWhile (/=' ') xs) : (myWords (dropWhile (/=' ') xs))

shouldEqual = [ "Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?"]

myLines :: String -> [String]
myLines [] = []
myLines ('\n':xs) = myLines xs
myLines xs = (takeWhile (/='\n') xs) : (myLines (dropWhile (/='\n') xs))

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
            == shouldEqual)

myGeneric s [] = []
myGeneric s (x:xs)
  | s == x = myGeneric s xs
  | otherwise = (takeWhile(/=s) (x:xs)) : (myGeneric s (dropWhile (/=s) (x:xs)))
