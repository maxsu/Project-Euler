---------------------------------------------
-- Max Suica, 04-14-2010. max.suica@gmail.com

import Data.List (permutations, sort)

nthPermutation n = (!!(n-1)) . sort . permutations $ "0123456789"

euler24 = nthPermutation 1000000

main :: IO ()
main = print euler24

