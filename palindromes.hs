alphabet = "1234567890"

pick1 :: String -> Int -> [String]
pick1 [] n     = []
pick1 (x:xs) n = replicate n x : pick1 xs n

pick2 alph = pick2' [] chosen
               where
             chosen = choosingTwo alph
             pick2' :: [String] -> [String] -> [String]
             pick2' res []     = res
             pick2' res (x:xs) = pick2' (pattern2 x ++ res) xs

pattern2 :: String -> [String]
pattern2 [x,y] = [[x,y,x,y,x], [x,y,y,y,x], [x,x,y,x,x]]

choosingTwo alph = [[x,y]| x <- alph, y <- alph, x /= y]

pick3 = undefined

-- To create a palindrome of length 5, we can pick one, two or three different characters from our alphabet
palindromes5 :: String -> [String]
palindromes5 alph = pick1 alph 5 ++ pick2 alph ++ pick3 alph
