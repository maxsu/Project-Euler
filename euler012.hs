---------------------------------------------
-- Max Suica, 03-10-2010. max.suica@gmail.com

-- Needs work

import Data.List (find)
import Data.Maybe (fromJust)
import Euler (factorcount)

   
divisors n = product.map ((+1).snd) $ factorcount n

triangleNums = scanl1 (+) [1..]

euler12 = snd.fromJust.find ((> 500).snd) $ map (\n-> (n, divisors n)) triangleNums
