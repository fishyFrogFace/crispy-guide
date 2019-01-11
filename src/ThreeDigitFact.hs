module ThreeDigitFact
  ( threeDigitFact ) where

import Permutations

digitCombinations = [[x,y,z]|x <- [0..9],y <- [0..9], z <- [0..9]]

threeDigitFact :: [[Int]] -> [Int]
threeDigitFact []     = []
threeDigitFact (l@[x,y,z]:xs)
  | number == factSum = l
  | otherwise         = threeDigitFact xs
      where
    number  = 100*x+10*y+z
    factSum = fromInteger $ factorial x + factorial y + factorial z

result = threeDigitFact digitCombinations
