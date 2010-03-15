---------------------------------------------
-- Max Suica, 02-10-2010. max.suica@gmail.com

import Data.List (sort)

palindrome a = 
  show a == (reverse $ show a)

palindromes min max =  
  [a*b | a <- ns, b <- ns, a <= b, palindrome (a*b)]
 where
  ns = [min..max]

euler4 = last $ sort (palindromes 100 999)

