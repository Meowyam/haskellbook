import Control.Applicative
import Data.Char
import Text.Trifecta

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show)

checkNumstr :: Parser NumberOrString
checkNumstr =
  try (fmap NOSI integer) <|> try (fmap NOSS $ some letter)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  release <- option [] $ char '-' >> checkNumstr `sepBy` (char '.')
  meta <- option [] $ char '+' >> checkNumstr `sepBy` (char '.')
  return $ SemVer major minor patch release meta
