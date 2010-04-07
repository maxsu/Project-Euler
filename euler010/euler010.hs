---------------------------------------------
-- Max Suica, 03-10-2010. max.suica@gmail.com

import Euler (primes)

euler10 = sum.takeWhile (<=2000000) $ (primes::[Int])


main :: IO ()
main = putStr $ show euler10
