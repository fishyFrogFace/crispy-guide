odds = "13579"
evens = "02468"

multiplesOf4 = map show [x*4|x <- [3..]]
otherNumbers = map show [x|x <- [10..], mod x 4 /= 0]

divPattern :: String -> Bool
divPattern [x,y]
  | elem y "048" && elem x evens = True
  | elem y "26" && elem x odds   = True
  | otherwise                    = False
divPattern (x:xs) = divPattern xs

checkValidUntil :: (a -> Bool) -> [a] -> Int -> Bool
checkValidUntil f list n = all f $ take n list

-- Test to check if all multiples of four >= 12 follows "divPattern" above
-- and that all other integers >= 10 does not
-- "both 100000" returns "True", which means this is valid for all six digit integers
-- this is adequate for the usage it was needed for
both :: Int -> Bool
both n = checkValidUntil divPattern multiplesOf4 n && 
         not (checkValidUntil divPattern otherNumbers n)
