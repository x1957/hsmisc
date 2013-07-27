
module Solution where

combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:s) = [x : y | y <- combinations (k-1) s ] ++ combinations k s 

