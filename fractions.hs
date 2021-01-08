{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"
testDec = "0.1"
testDec2 = "2.5"
testNo = "2"

getDecimal n =
  n % divi
    where divi = ((^) 10) . length . show $ n

parseDecimal :: Parser Rational
parseDecimal = do
  num <- decimal
  char '.'
  denom <- decimal
  return $ (num % 1) + (getDecimal denom)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do
  let virtuousFraction' =
        parseString virtuousFraction mempty
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork

testBoth :: Parser Rational
testBoth = try virtuousFraction <|> try parseDecimal

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

main :: IO ()
main = do
--  let parseFraction' =
--        parseString parseFraction mempty
  let testBoth' =
        parseString testBoth mempty

--  print $ parseFraction' shouldWork
--  print $ parseFraction' shouldAlsoWork
  print $ testBoth' shouldWork
  print $ testBoth' shouldAlsoWork
  print $ testBoth' testDec
  print $ testBoth' testDec2 
  print $ testBoth' "2.0"
