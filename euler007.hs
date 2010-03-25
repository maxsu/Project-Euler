---------------------------------------------
-- Max Suica, 03-10-2010. max.suica@gmail.com

import Euler (primes)

euler7 = primes !! 10000


main :: IO ()
main = putStr $ show euler7