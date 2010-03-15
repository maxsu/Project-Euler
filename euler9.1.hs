---------------------------------------------
-- Max Suica, 02-10-2010. max.suica@gmail.com


import List (find)
import Maybe (fromJust)
import qualified Data.MemoCombinators as Memo

square x = x == (floor . sqrt . fromIntegral $ x)^2

pythagorean = [ [a,b,c] | height <- [1..], 
                            a <- [1..div height 2],
						    let b = height - a,
						    gcd a b == 1,
						    square (a^2 + b^2),
						    let c = floor . sqrt . fromIntegral $ (a^2 + b^2)]

triple n = fromJust . find ((== 0). (rem n) . sum) $ pythagorean
scale = div 1000 $ sum triple

euler9 = product . map (*scale) $ triple 
