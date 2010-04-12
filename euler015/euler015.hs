---------------------------------------------
-- Max Suica, 04-12-2010. max.suica@gmail.com

fac = product . enumFromTo 1

paths n = fac (2*n) `div` (2 * fac n)

euler15 = paths 20

main :  IO ()
main = print euler15

