{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

--Q1
mult10 :: [Int] -> [Int]
mult10 = map (*10)

--Q2
onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

onlyUpperCase :: String -> String
onlyUpperCase = filter isUpper

--Q3



--Q4



--Q5
zeroToTen :: [Int] -> [Int]
zeroToTen = filter numRange
    where
        numRange x = x >= 0 && x <= 10

--Q6
squareRoots :: [Float] -> [Float]
squareRoots = map sqrt . filter (> -1) -- . Composite Function

timesTenFromEven :: [Int] -> [Int]
timesTenFromEven = map (*10) . filter even


--Q7
 



 --Q8



--Q9    