import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = rev <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled = do
  a <- cap
  b <- rev
  return (a, b)

tupled' :: [Char] -> ([Char], [Char])
tupled' =
  rev >>= \a
  -> cap >>= \b
  -> return (a, b)
