---------------------------------------------
-- Max Suica, 03-10-2010. max.suica@gmail.com

-- Needs work

import Data.List (find)
import Data.Maybe (fromJust)
import Euler (factorCount)

   
divisors n = product . map ((+1).snd) $ factorCount n

triangleNums = scanl1 (+) [1..]

euler12 = fromJust.find ((> 500).divisors) triangleNums


main :: IO ()
main = putStr $ show euler12
