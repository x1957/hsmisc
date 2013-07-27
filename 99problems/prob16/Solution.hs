
module Solution where

dropEvery l n = map (\(_, x) -> x) 
    $ filter (\(i, _) -> mod i n /= 0) 
    $ zip [1 .. ] l
