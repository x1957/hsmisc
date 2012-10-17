msort [] = []
msort [x] = [x]
msort xs
  | size > 0 = merge (msort front) (msort back)
  where size = length xs `div` 2
        front = take size xs
        back = drop size xs

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | x > y = y: merge (x:xs) ys

main = do
  let ll = [5,4,3,6,1,2,3,7,3,10]
  putStrLn $ show $ msort ll
  