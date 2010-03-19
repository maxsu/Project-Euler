module Euler where

-- ^ Integer square root function (not accurate above 10^20).
intsqrt = floor.sqrt.fromIntegral

-- ^ Tests whethere a number is a perfect square.
square x = x == intsqrt x ^ 2

-- ^ List of primes by trial division
primes = 2:[p | p <- [3,5..], prime p]

-- ^ Basic primality testing by trial division
prime a = all (notDiv a) $ takeWhile (<= (intsqrt a))  primes
 where 
   notDiv n p = n `mod` p /= 0

-- ^ List of a number's prime factors. Example: factor 12 = [2, 2, 3].
factor n = factor' n [] primes
 where
   factor' n ans (p:ps) 
     | n < p        = reverse ans
     | mod n p == 0 = factor' (div n p) (p:ans) (p:ps)
	 | otherwise    = factor' n ans ps

-- ^ List of primitive pythagorean triples
pythagorean = [[m^2-n^2, 2*m*n, m^2+n^2]| m <- [2..], n <- [1..m-1],
                                            even m /= even n, gcd m n == 1]


-- These need work     
factorcount n = fc fs [(f,1)]
 where 
   (f:fs) = factor n
   fc [] ps = reverse ps
   fc (fac:facs) ((f1,n):pairs) 
     | fac == f1 = fc facs ((f1, n+1): pairs)
     | otherwise = fc facs ((fac, 1): (f1, n): pairs)

     
divisors n = product.map ((+1).snd) $ factorcount n