---------------------------------------------
-- Max Suica, 02-10-2010. max.suica@gmail.com

import Data.List (find)

primes :: Integral a => [a]
primes = 2:[p | p <- [3,5..], prime p]
 where
  prime p = find ((==0).(rem p)) (takeWhile (<= (floor . sqrt . fromIntegral $ p))  primes) == Nothing
euler7 = primes !! 10000