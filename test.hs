init1 :: String -> String -> String
init1 firstname lastname = [f] ++ "." ++ [l] ++"."
  where (f:_) = firstname
        (l:_) = lastname