{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)
import Distribution.Compat.Lens (_1)
import Distribution.Simple.Utils (xargs)

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x     
snd (_,y)       = y      

-- Definitions of the prelude functions head and tail

head (x:_)      = x     
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

--Q1
headPlusOne :: [Int] -> Int
headPlusOne [] = - 1
headPlusOne (x:_) = x + 1

--Q2
duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x:x:xs

--Q3
rotate :: [a] -> [a]
rotate (x:y:xs) = y:x:xs
rotate xs        = xs

-- recursion

--Q4
listLength :: [a] -> Int
listLength []           = 0
listLength (x:xs)       = 1 + listLength xs 


{-

[0,4,2,10,56]
x:xs            -- intial x=0, initial xs=[4,2,10,56]

1 + listLength [4,2,10,56]      -- new x=4, new xs=[2,10,56]
1 + 1 + listLemgth [2,10,56]    -- next new x=2, next new xs=[10,56]
1 + 1 + 1 + listLength [20,56]
1 + 1 + 1 + 1 + listLength [56]    -- next new x=56, next new xs=[]
1 + 1 + 1 + 1 + 1 + listLength []
1 + 1 + 1 + 1 + 1 + 0

-}

--Q5
multAll :: [Int] -> Int
multAll []              = 1
multAll (x:xs)          = x * multAll xs

{-
[2,3,5]
x:xs      -- initial x=2, initial xs=[3,5]

2 * multiAll [3,5]        -- new x=3, new xs=[5]
2 * 3 * multAll [5]
2 * 3 * 5 * multAll []
2 * 3 * 5 * 1

-}

--Q6

andAll :: [Bool] -> Bool
andAll []                 = True
andAll (x:xs)             = x && andAll xs

{-
[True,True,False]
x:xs      -- initial x=T, initial xs=[T,F]

T && andAll [T,F]        -- new x=T, new xs=[F]
T && T andAll [F]        -- next new x=[F], next new xs=[]
T && T && F andAll []
T && T && F && T
T && F && T
F && T
F

-}

--Q7
orAll :: [Bool] -> Bool
orAll [] = False  
orAll (x:xs) = x || orAll xs 


--Q8
countIntegers :: Int -> [Int] -> Int
countIntegers _ []              = 0  -- base case: empty list returns 0
countIntegers n (x:xs)
    | n == x                    = 1 + countIntegers n xs  -- if current element matches, add 1 and recurse
    | otherwise                 = countIntegers n xs       -- if no match, just recurse

{-
3   [5, 3, 8, 3, 9]

3   5:[3,8,3,9]

3 == 5 ?  no  countIntegers 3 [3,8,3,9]

3  3:[8,3,9]

3 == 3 yes      1 + countIntegers 3 [8,3,9]

3 8:[3,9]

3 == 8 ? no     1 + countIntegers 3 [3,9]

3 3:[9]

3 == 3  yes     1 + 1 + countIntegers 3 [9] 

3  9:[]

3 == 9 ? no     1 + 1 + countIntegers 3 []

3 []            1 + 1 + 0 

                  = 2

-}

countEven :: [Int] -> Int
countEven []              = 0  -- base case: empty list returns 0
countEven (x:xs)
    | x `mod` 2 == 0            = 1 + countEven xs  -- if current element matches, add 1 and recurse
    | otherwise                 = countEven xs       -- if no match, just recurse


--Q9
removeAll :: Int -> [Int] -> [Int]
removeAll _ []                  = []
removeAll n (x:xs)
    | n == x                    = removeAll n xs -- Skips head if n == head
    | otherwise                 = x : removeAll n xs --Prepends head to a new list
    

--Q10
removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst _ []                      = []
removeAllButFirst n (x:xs)
    | n == x                    = x : removeAll n xs
    | otherwise                 = removeAllButFirst n xs


--Q11
type StudentMark = (String, Int)

testData :: [StudentMark]
testData =
    [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
    ]


--Q11
listMarks :: String -> [StudentMark] -> [Int]
listMarks _ []              = [] 
listMarks str (x:xs)
    | str == fst x          = snd x : listMarks str xs
    | otherwise             = listMarks str xs 

--Q12
sorted :: [Int] -> Bool
sorted []               = True -- checks if list is empty
sorted [_]              = True -- checks if only 1 value left and there is  othing to compare 
sorted (x:y:xs)
    | x <= y            = sorted (y : xs)
    | otherwise         = False -- If the list isnt sorted

--Q13
prefix :: [Int] -> [Int] -> Bool
prefix [] _               = True -- Checks the true condition if reach the end of the first list and is still a prefix to the scond list we are true
prefix _ []               = False -- Checks if firt list is bigger than second list
prefix (x:xs) (y:ys)
    | x == y              = prefix xs ys 
    | otherwise           = False -- if they dont match its false

--Q14
subSequence :: [Int] -> [Int] -> Bool
subSequence [] _            = True
subSequence _ []            = False
subSequence xs (y:ys)
    | prefix xs (y:ys)      = True
    | otherwise             = subSequence xs ys  