module LearnParsers where
import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' = one >> stop

one'' = one >> eof

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

oneTwo'' = oneTwo >> eof 

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParse'' :: Parser () -> IO ()
testParse'' p =
  print $ parseString p mempty "123"

makeStr :: String -> Parser String
makeStr s = string s

makeStr' s = (makeStr s) >> stop

p123 :: Parser String -> IO ()
p123 p =
  print $ parseString p mempty "123"

stringChar :: String -> Parser String
stringChar (x:xs) = go (x:xs) mempty
  where
    go [] allChar = return allChar
    go (x:xs) allChar = char x >>= (\eachChar -> go xs (allChar ++ [eachChar]))

pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one
  
  pNL "one':"
  testParse one'

  pNL "one eof:"
  testParse'' one''

  pNL "oneTwo:"
  testParse oneTwo
  
  pNL "oneTwo':"
  testParse oneTwo'

  pNL "oneTwo eof:"
  testParse'' oneTwo''

  pNL "oneTwoThree:"
  p123 (makeStr "1")
  p123 (makeStr "12")
  p123 (makeStr "123")
  p123 (makeStr' "1")
  p123 (makeStr' "12")
  p123 (makeStr' "123")
  p123 (stringChar "123")
