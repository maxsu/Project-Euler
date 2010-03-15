---------------------------------------------
-- Max Suica, 02-10-2010. max.suica@gmail.com

sum1 n = n * (n + 1) `div` 2
sum2 n = n * (n + 1) * (2*n + 1) `div` 6
euler6 = (sum1 100)^2 - sum2 100
