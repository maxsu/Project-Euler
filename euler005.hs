---------------------------------------------
-- Max Suica, 03-10-2010. max.suica@gmail.com


euler5 = foldl lcm 1 [1..20]


main :: IO ()
main = putStr $ show euler5