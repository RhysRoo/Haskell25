data List = Null | Node Int List
    deriving (Show)

testList = Node 2 (Node 3 (Node 5 Null))

-- Pattern matching (Only adds int to head of list)
process :: Int -> List -> List
process x Null                  = Null
process x (Node y ys)           = Node (x + y) ys

-- Recursive function (Keeps adding int to every new head after recurisve call until Null)
process2 :: Int -> List -> List
process2 x Null                  = Null
process2 x (Node y ys)           = Node (x + y) (process2 x ys)
