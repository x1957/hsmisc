
module Solution where

removeAt n l = 
    let al = zip [1 .. ] l
        x = lookup n al
        z = filter (\(i, _) -> i /= n) al
    in  case x of
            Just y -> (y, map (\(_, x) -> x) z)


