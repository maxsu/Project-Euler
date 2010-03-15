-- Project Euler Problem 89

import List (find)
import Maybe (fromJust)
import qualified Data.MemoCombinators as Memo

square x = x == (floor . sqrt . fromIntegral $ x)^2

rooms n = 
    [(x,y,z) | x <- a, y <- a, z <- a, x <= y, y <= z] 
	where a = [1..n]

pythagorean n = [(a,b) | a <- [1..2*n], b <- [1..n], a <= 2*b, square (a^2 + b^2)]

rooms2 n = 
    [(x,y,k) | (j,k) <- pythagorean n, x <- [1..j-1], y <- [1..k], x <= y, x + y == j, y <= k   ] 
	where a = [1..n]

test n = foldl (\ x (a,b) -> x + 1 + (div a 2) - (min 1 (a - b))) 0 (pythagorean n)

path (x,y,z) = (x + y)^2 + z^2

count n = length $ filter (square . path) (rooms n)

-- count 100 == 2060
-- count 99  == 1975

counts n = takeWhile (<= n) (map count [1..])