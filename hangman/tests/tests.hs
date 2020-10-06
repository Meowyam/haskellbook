module Test where

import Hangman
import Test.Hspec
-- test word "meow"

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "fills in a character" $ do
      let puzzle :: Puzzle
          puzzle = Puzzle "meow" [] []

          expected :: Puzzle
          expected = Puzzle "meow" [] ['m']
      
      fillInCharacter puzzle 'm' `shouldBe` expected
  describe "handleGuess" $ do
    it "handles a guess" $ do
      let puzzle :: Puzzle
          puzzle = Puzzle "meow" [] []

          expected :: Puzzle
          expected = Puzzle "meow" [] ['m']
    
      actual <- handleGuess puzzle 'm'
      actual `shouldBe` expected