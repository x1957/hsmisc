module Solution where
import Data.List (nub)

phi m = foldl f m $ pfs m where
  f n p = n - (div n p)
  pfs m = filter (/=1) $ nub $ pfs' 2 [] m where
    pfs' k ps m | k * k > m = m : ps
                | mod m k == 0 = pfs' k (k : ps) (div m k)
                | otherwise = pfs' (k + 1) ps m
