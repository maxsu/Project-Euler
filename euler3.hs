---------------------------------------------
-- Max Suica, 02-10-2010. max.suica@gmail.com

import Data.List (unfoldr)

primes = 2:[p | p <- [3,5..], prime p]
 where
  prime a = all (notDiv a) $ takeWhile (<= (floor.sqrt.fromIntegral $ a))  primes
  notDiv n p = n `mod` p /= 0

factor n = factor' n [] primes
 where
   factor' n ans (p:ps) 
     | n < p        = reverse ans
     | rem n p == 0 = factor' (div n p) (p:ans) (p:ps)
	 | otherwise    = factor' n ans ps

factor2 n = unfoldr test (n, primes)
 where
  test (n, (p:ps)) 
    | n < p = Nothing
    | rem n p == 0 = Just (p, (div n p, (p:ps)))
	| otherwise = test (n, ps)

testNum = 600851475143
euler3 = last.factor $ testNum