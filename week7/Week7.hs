-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
           deriving (Eq,Ord,Show,Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat  = True
isWeekend Sun  = True
isWeekend _    = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
     deriving (Eq,Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)  
    | m1 >= m2          = s1
    | otherwise         = s2

-- Shapes algebraic type 
data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
               deriving (Show)

data Building = Name String | 
                Number Int
                deriving (Show)

-- Binary tree algebraic type
data Tree = Null | 
     Node Int Tree Tree
     deriving (Show)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
                  (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree =  Node 5 (Node 1 Null Null)
                         (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2

--Q1/Q2
data Month = January | February | March | April | May | June | July | August | September | October | November | December
     deriving (Show, Read, Eq, Ord)

data Seasons = Winter | Spring | Summer | Autumn
     deriving (Show, Read, Eq, Ord)

season :: Month -> Seasons
season m 
     | m == December || m == January || m == February       = Winter
     | m >= March && m <= May                               = Spring
     | m >= June && m <= August                             = Summer
     | m >= September && m <= November                      = Autumn
     | otherwise                                            = error "Out of range"


--Q3
numberOfDays :: Month -> Int -> Int
numberOfDays m y
     | m == February && (y `mod` 4 /= 0)                                                                           = 28
     | m == February && (y `mod` 4 == 0)                                                                           = 29
     | m == January || m == March || m == May || m == July || m == August || m == October || m == December         = 31
     | m == April || m == June || m == September || m == November                                                  = 30


--Points and Shapes
--Q4 
data Point = PointX | PointY
     deriving (Show)

--Q5
data PositionedShape = Shape | Point
     deriving (Show, Read)

--Q6
--move :: PositionedShape -> Float -> Float -> PositionedShape

-- Functions for the binary tree

--Q7
numberOfNodes :: Tree -> Int
numberOfNodes Null                           = 0
numberOfNodes (Node nodeVal left right)      = 1 + numberOfNodes left + numberOfNodes right 

{-
         20
       /    \
      3     8
     / \    /
   12   7  4
          /
         6


1.   1 + nunmberOfNodes '3'                                                                                 + numberOfNodes '8' 
2.   1 + 1 + numberOfNodes '12'                           + numberOfNodes '7'                               + 1 + numberOfNodes '4' (left)                         + numberOfNodes Null (right)
3.   1 + 1 + 1 + numberOfNodes Null + numberOfNodes Null  + 1 + numberOfNodes Null + numberOfNodes Null     + 1 + 1 + numberOfNodes '6' + numberOfNodes Null       +       0
4.   1 + 1 + 1 +      0             +       0             + 1 +        0           +       0                + 1 + 1 + 1 + numberOfNodes Null + numberOfNodes Null  +       0            +   0
5.   1 + 1 + 1 +      0             +       0             + 1                                               + 1 + 1 + 1 +       0            +       0             +       0            +   0
6.                            = 7

-}

--Q8
isMember :: Int -> Tree -> Bool
isMember _ Null                         = False
isMember n (Node nodeVal left right)
     | n == nodeVal                     = True 
  -- | otherwise                        = False || isMember n left || isMember n right
     | otherwise                        = isMember n left || isMember n right


--Q9

leaves :: Tree -> [Int]
leaves Null                        = []
leaves (Node nodeVal Null Null)    = [nodeVal]
leaves (Node nodeVal left right)   = leaves left   ++   leaves right



--Q10
