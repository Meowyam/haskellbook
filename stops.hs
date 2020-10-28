import Control.Applicative (liftA3)

-- http://cmsc-16100.cs.uchicago.edu/2016/Lectures/14-applicative.php

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos a b c = liftA3 (,,) a b c

--nouns = ["cat", "bear"]
--verbs = ["pet", "lick"]

--stopVowelStop = [(stop1, vowel, stop2) | stop1 <- stops, vowel <- vowels, stop2 <- stops] 
--pVowelStop = [(stop1, vowel, stop2) | stop1 <- stops, vowel <- vowels, stop2 <- stops, stop1 == 'p'] 
--nounVerbNoun = [(n1, v, n2) | n1 <- nouns, v <- verbs, n2 <- nouns] 
