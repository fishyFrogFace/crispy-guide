module Subsets 
  ( subsets
  , subsetLength ) where

import Control.Monad

data Binary = One | Zero deriving (Show, Eq)

permutations :: Int -> [[Binary]]
permutations n = take (2^n) $ iterate next (replicate n Zero)

next :: [Binary] -> [Binary]
next [] = []
next lst@(x:xs)
  | x == Zero = One:xs
  | x == One  = insertOne lst

insertOne :: [Binary] -> [Binary]
insertOne (One:Zero:xs) = Zero:One:xs
insertOne (x:xs)        = Zero : insertOne xs

subsets :: [a] -> [[a]]
subsets lst = map (subsets' lst) (permutations lenlst)
                where
                subsets' :: [a] -> [Binary] -> [a]
                subsets' [] [] = []
                subsets' (x:xs) (y:ys)
                  | y == One  = x:subsets' xs ys
                  | y == Zero = subsets' xs ys
                lenlst = length lst

subsetLength :: Int -> [a] -> [[a]]
subsetLength n set = filter (\x -> length x == n) $ subsets set

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
  | elem x xs = unique xs
  | otherwise = x : unique xs
