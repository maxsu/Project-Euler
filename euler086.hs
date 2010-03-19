---------------------------------------------
-- Max Suica, 03-06-2010. max.suica@gmail.com


-- Definitely not done

import List (find)
import Maybe (fromJust)
import qualified Data.MemoCombinators as Memo

square x = x == (floor . sqrt . fromIntegral $ x)^2

rooms n = 
    [(x,y,z) | z <- [1..n], y <- [1..z], x <- [1..y]] 

intsqrt:: Int -> Int
intsqrt x = floor.sqrt.fromIntegral $ x

lt:: Integral a => a -> Float -> Bool
lt a b = (fromIntegral a) < b 

pythagorean n = [(a,b) | b <- [1..n], a <- [1..b], square (a^2 + b^2)]
primitives n =    [(a,b) | b <- [1..n], a <- [1..b], gcd a b == 1, square (a^2 + b^2)]
primitives2 lim =   [(a,b,c) | m <- [2 .. intsqrt (lim-1)], 
                               n <- [1 .. min (m-1) (intsqrt(lim - m^2))], 
                               even m /= even n, 
                               gcd m n == 1,
                               let a' = 2*m*n,
                               let b' = m^2 - n^2,
                               let c = m^2 + n^2,
                               let a = min a' b',
                               let b = max a' b']

primitives3 lim =   [(a,b,c) | m <- [2 .. intsqrt (lim-1)], 
                               n <- [1 .. lim], 
                               even m /= even n, 
                               gcd m n == 1,
                               lt n ((sqrt 2 - 1) * fromIntegral m),
                               let a = 2*m*n,
                               let b = m^2 - n^2,
                               let c = m^2 + n^2  ]

primitives4 lim =   [(a,b,c) | m <- [2 .. 2*lim], 
                               n <- [1 .. min m (div lim (2*m))], 
                               even m /= even n, 
                               gcd m n == 1,
                               lt (-n) (-(sqrt 2 - 1) * fromIntegral m),
                               let a = m^2 - n^2,
                               let b = 2*m*n,
                               let c = m^2 + n^2  ]

rooms2 n = 
    [(x,y,k) | (j,k) <- pythagorean n, x <- [1..j-1], y <- [1..k], x <= y, x + y == j, y <= k   ] 
	where a = [1..n]

test n = foldl (\ x (a,b) -> x + 1 + (div a 2) - (min 1 (a - b))) 0 (pythagorean n)

path (x,y,z) = (x + y)^2 + z^2

count n = length $ filter (square . path) (rooms n)

-- count 100 == 2060
-- count 99  == 1975

counts n = takeWhile (<= n) (map count [1..])