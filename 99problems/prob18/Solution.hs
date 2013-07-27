
module Solution where


slice l i j = take (j - i + 1) . drop (i - 1) $ l

