
module Solution where

data Code a = Single a | Multiple Int a
    deriving Show

decodeModified code = concat $ map f code
    where f c = case c of { Single a -> [a]; Multiple t a -> replicate t a }

