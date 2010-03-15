---------------------------------------------
-- Max Suica, 02-10-2010. max.suica@gmail.com


primes = 2:[p | p <- [3,5..], prime p]
 where
  prime a = all (notDiv a) $ takeWhile (<= (floor.sqrt.fromIntegral $ a))  primes
  notDiv n p = n `mod` p /= 0

euler7 = primes !! 10000