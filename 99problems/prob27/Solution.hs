
module Solution where
import Data.List( (\\) )

choose _ 0 = [[]]
choose [] _ = []
choose (x:s) k = [x : y | y <- choose s (k - 1)] ++ choose s k

group [k] s = map (\x -> [x]) (choose s k)
group (k:p) s = 
    let c1 = choose s k 
    in  [ g1 : gn | g1 <- c1, gn <- (group p (s \\ g1)) ]
