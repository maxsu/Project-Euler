---------------------------------------------
-- Max Suica, 02-10-2010. max.suica@gmail.com



primes = 2 : 3 : filter prime [5,7..]
 where
  prime n  = all (notDiv n) (takeWhile (\ p -> (p*p) <= n)  primes)
  notDiv n p = mod n p /= 0


main :: IO ()
main = putStr $ show $ primes!!10000