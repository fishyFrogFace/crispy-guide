factorials :: [Integer]
factorials = 1:zipWith (*) factorials [1..]

-- Ways to place n distinct objects into n distinct boxes,
-- e.g. possible 2-byte words, permutations of the letters in CUPHOLDER
factorial :: Int -> Integer
factorial n = factorials !! n

-- If you have n distinct objects that are going to be put in k boxes
-- n >= k
kPermutationsOfN :: Int -> Int -> Integer
kPermutationsOfN n k = div (factorial n) (factorial $ n-k)

-- Permutations of n objects where one or more are repeated,
-- e.g. permutations of the word AARDWARK
repetitions :: Int -> [Int] -> Integer
repetitions n reps = factorial n `div` rep reps
                      where
                     rep :: [Int] -> Integer
                     rep []     = 1
                     rep (x:xs) = factorial x + rep xs
