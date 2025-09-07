import Prelude hiding (concat, fst, head, reverse, snd, sum, tail, zip)

{- lecture Notes
-}

-- example 1
describeShape :: Int -> String
describeShape 3 = "triangle"
describeShape 4 = "quadrilatrial"
describeShape _ = "unknown"

fst' (x, _) = x

null' :: [a] -> Bool
null' [] = True -- null' [] is a patten
null' _ = False

 This file illustrates list patterns and recursion over lists.
-}

-- Definitions of the prelude functions fst and snd

fst (x, _) = x

snd (_, y) = y

-- Definitions of the prelude functions head and tail

head (x : _) = x

tail (_ : xs) = xs

absFirst :: [Int] -> Int
absFirst [] = -1
absFirst (x : xs) = abs x

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x : xs) = 2 * x : doubleAll xs

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _ _ = []

-- Worked example 1
countSpaces :: String -> Int
countSpaces "" = 0
countSpaces (x : xs)
    | x == ' ' = 1 + countSpaces xs
    | otherwise = countSpaces xs

-- Worked example 2
mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
    | x <= y = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

-- Start of Questions

-- Question 1
headPlusOne :: [Int] -> Int
headPlusOne [] = error "Empty"
headPlusOne (x:_) = x + 1

-- Question 2
duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x : x : xs

-- Question 3
rotate :: [a] -> [a]
rotate (x1:x2:xs) = x2:x1:xs
rotate xs = xs

--Question 4
    