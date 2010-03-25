---------------------------------------------
-- Max Suica, 02-10-2010. max.suica@gmail.com


import List (find)
import Maybe (fromJust)
import Euler (pythagorean)

triple = fromJust.find (\x -> sum x `mod` 1000 == 0) $ pythagorean
scale = div 1000 (sum triple)

euler9 = product . map (*scale) $ triple


main :: IO ()
main = putStr $ show euler9