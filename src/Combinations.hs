module Combinations
  ( nChooseK ) where

import Permutations

nChooseK :: Int -> Int -> Integer
nChooseK n k = factorial n `div` (factorial k * factorial (n-k))
