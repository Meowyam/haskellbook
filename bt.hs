{-# LANGUAGE OverloadedStrings #-}

module BT where

import Control.Applicative
import qualified Data.Attoparsec.ByteString
  as A
import Data.Attoparsec.ByteString
  (parseOnly)

import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

trifP :: Show a
      => Parser a
      -> String -> IO ()
trifP p i =
  print $ parseString p mempty i

-- runs a trifecta parser and prints the result
parsecP :: (Show a)
        => Parsec String () a
        -> String -> IO ()
parsecP = parseTest

-- runs a parsec parser and prints the result
attoP :: Show a
      => A.Parser a
      -> ByteString -> IO ()
attoP p i =
  print $ parseOnly p i 

-- helper function for Attoparsec
nobackParse :: (Monad f, CharParsing f)
            => f Char
nobackParse =
       (char '1' >> char '2')
   <|> char '3'

-- Parser that attempts to parse 'a' then '2' or '3'
-- This parser does not backtrack
tryParse :: (Monad f, CharParsing f)
         => f Char
tryParse =
       try (char '1' >> char '2')
   <|> char '3'

main :: IO ()
main = do
  -- trifecta
  trifP nobackParse "13"
  trifP nobackParse "12" -- should succeed
  trifP nobackParse "3" -- should succeed
  trifP tryParse "13"
  trifP tryParse "12" -- should succeed
  trifP tryParse "3" -- should succeed

  -- parsec
  parsecP nobackParse "13"
  parsecP nobackParse "12" -- should succeed
  parsecP nobackParse "3" -- should succeed
  parsecP tryParse "13"
  parsecP tryParse "12" -- should succeed
  parsecP tryParse "3" -- should succeed

  -- attoparsec
  attoP nobackParse "13"
  attoP nobackParse "12"
  attoP nobackParse "3"
  attoP tryParse "13"
  attoP tryParse "12"
  attoP tryParse "3"

-- it can be hard to identify which parse caused which error when you have backtracking in a parser
-- This can be avoided by using the <?> operator to annotate parse rules whenever you use try
tryAnnot :: (Monad f, CharParsing f)
         => f Char
tryAnnot =
       (try (char '1' >> char '2')
       <?> "Tried 12")
   <|> (char '3' <?> "Tried 3")
