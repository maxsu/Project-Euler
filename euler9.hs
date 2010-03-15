---------------------------------------------
-- Max Suica, 02-10-2010. max.suica@gmail.com


import List (find)
import Maybe (fromJust)
import qualified Data.MemoCombinators as Memo

square x = x == (floor . sqrt . fromIntegral $ x)^2

pythagorean n = [ [a,b,c] | a <- [1..n], 
                           b <- [1..n],
						   a <= b,
						   gcd a b == 1,
						   square (a^2 + b^2),
						   let c = floor . sqrt . fromIntegral $ (a^2 + b^2)]

triple = fromJust . find ((== 0). (rem 1000) . sum) $ pythagorean 20
scale = div 1000 $ sum triple

euler9 = product . map (*scale) $ triple 
