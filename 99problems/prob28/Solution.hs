
module Solution where

lsort l = map (\(_, x) -> x) sl
    where
        ll = map (\l -> (length l, l)) l 
        sl = sort ll
            where 
                sort [] = []
                sort (x:y) = l ++ [x] ++ r
                    where
                        l = sort [z | z <- y, z <= x]
                        r = sort [z | z <- y, z > x] 



