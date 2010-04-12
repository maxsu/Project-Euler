---------------------------------------------
-- Max Suica, 04-12-2010. max.suica@gmail.com

import Data.Time

a = fromGregorian 1901 1 1

b = fromGregorian 2000 12 31

firsts = filter ((\(_,_,a) -> a == 1).toGregorian) [a..b]

firstSundays = filter (\x -> toModifiedJulianDay x `mod` 7 == 4) firsts

euler19 = length firstSundays

main :: IO ()
main = print euler19

