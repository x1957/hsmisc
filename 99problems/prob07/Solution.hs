
module Solution where

data NestedList a = Elem a | List [NestedList a]

flatten a = 
    case a of
        Elem a -> [a]
        List a -> foldl (++) [] (map flatten a)

