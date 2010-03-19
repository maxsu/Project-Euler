---------------------------------------------
-- Max Suica, 03-17-2010. max.suica@gmail.com

-- Definitely not done!

import qualified Data.MemoCombinators as Memo

collatz n = iterate (\ x -> if even x then div x 2 else 3*x+1) n


collatzcount n = 1 + length (takeWhile (/=1) $ collatz n)

collatzcount2 = Memo.integral collatzcount'
 where
   collatzcount' n = count' 0 n
   count' c 1 = 0
   count'  c n = 1 + collatzcount' ((\ x -> if even x then div x 2 else 3*x+1) n)

maximum' a = max' a 0
 where 
  max' (a:as) n = seq n $ max' as (max a n)
  max' []  n = n

euler14 = maximum' . map collatzcount $ [1..1000000]
