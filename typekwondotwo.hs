chk :: Eq b => (a -> b) -> a -> b -> Bool

chk x y z = x y == z

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b

-- force to be integer with fromInteger
arith x y z = fromInteger y + (x z)
