---------------------------------------------
-- Max Suica, 03-10-2010. max.suica@gmail.com


nsum n k = k * a * (a + 1) `div` 2 
  where a = div n k

euler1 = f 3 + f 5 - f 15
  where f = nsum 1000


main :: IO ()
main = putStr $ show euler1