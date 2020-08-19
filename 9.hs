safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs

--true is higher than false
eftBool :: Bool -> Bool -> [Bool]
eftBool True False = []
eftBool False True = [False, True]
eftBool True True = [True]
eftBool False False = [False]

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
eftGen x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : (eftGen (succ x) y) 
