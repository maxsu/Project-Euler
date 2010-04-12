---------------------------------------------
-- Max Suica, 04-12-2010. max.suica@gmail.com

import Euler
import Control.Monad (ap)
import Control.Monad.Instances


-- let d(n) be defined as the sum of proper divisors of n
-- note: d(n) = \prod \frac{p_k^{n_k + 1} - 1}{p_k - 1}

multiPowSet [] = [[]]
multiPowSet ((x,n):xs) =
  ap (++) (flip map [1..n] . flip ((:) . (,) x) =<<) (multiPowSet xs)

d :: Integer -> Integer
d n = (+(- n)) . sum . map prod' . multiPowSet $ factorCount n
 where
  prod' = product . map (\(b,p) -> b^p)

amicable n = (d.d) n == n && n /= d n

euler21 = sum . filter amicable $ [2..10000]

main :: IO ()
main = print euler21

