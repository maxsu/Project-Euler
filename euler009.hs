---------------------------------------------
-- Max Suica, 02-10-2010. max.suica@gmail.com


import List (find)
import Maybe (fromJust)
import Euler (pythagorean)


triple = fromJust.find ((== 0).(rem 1000).sum) $ pythagorean
scale = div 1000 (sum triple)

euler9 = product . map (*scale) $ triple 
