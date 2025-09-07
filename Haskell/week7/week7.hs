-- 4. version 2

data Point2 = Point2 Float Float
  deriving (Show)

-- creating instance of Point p1:
p1 :: Point2
p1 = Point2 2.0 3.0

-- 5. version 2

-- Shapes algebraic type
data Shape
  = Circle Float
  | Rectangle Float Float
  deriving (Show)

data PositionedShape2 = PositionedShape2 Shape Point2
  deriving (Show)

-- example (instance of PositionedShape):
circle :: Shape
circle = Circle 30.0

centre :: Point2
centre = Point2 50 10

positionedCircle :: PositionedShape2
positionedCircle = PositionedShape2 circle centre

-- 6.

move :: PositionedShape2 -> Float -> Float -> PositionedShape2
move (PositionedShape2 shape (Point2 x y)) dx dy = PositionedShape2 shape (Point2 (x + dx) (y + dy))

-- testing example:
-- move positionedCircle 20 20
-- move positionedCircle 20 (-10)

--- 7

-- Binary tree algebraic type
data Tree
  = Null
  | Node Int Tree Tree
  deriving ()

-- Binary tree test data
testTree = Node 20 
            (Node 3 
            (Node 12 Null Null) 
            (Node 7 Null Null)
            )

            (Node 8 
              (Node 4 
                (Node 6 Null Null)
                Null
              ) 
            Null
          )

numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node value leftTree rightTree) = 1 + numberOfNodes leftTree + numberOfNodes rightTree
--numberOfNodes (Node _ leftTree rightTree) = 1 + numberOfNodes leftTree + numberOfNodes rightTree

---8
isMember :: Int -> Tree -> Bool
isMember _ Null = False
isMember targetValue (Node value leftTree rightTree) = targetValue == value || isMember targetValue leftTree || isMember targetValue rightTree

---9
leaves :: Tree -> [Int]
leaves Null = []
leaves (Node value Null Null) = [value]
leaves (Node value leftTree rightTree) = leaves leftTree ++ leaves rightTree
-- leaves (Node _ leftTree rightTree) = leaves leftTree ++ leaves rightTree

----10
inOrder :: Tree -> [Int]
inOrder Null                                = []
inOrder (Node value leftTree rightTree)     = inOrder leftTree ++ [value] ++ inOrder rightTree

---- 11

