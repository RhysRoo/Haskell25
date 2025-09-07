import Prelude hiding ((||), (&&), gcd)


--1
infixr 3 &&


(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

--2

exOr :: Bool -> Bool -> Bool
exOr False False = False
exOr True True = False
exOr _ _ = True

--3
ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True a _ = a
ifThenElse False _ b = b




--4
daysInMonth :: Int -> Int
daysInMonth < 1  = 0
daysInMonth > 12 = 0
daysInMonth 1 = 31
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30 
daysInMonth 11 = 30
daysInMonth _ = 31

--- 5
sumNumbers :: Int -> Int
sumNumbers n
    | n == 0          = 0 
    | otherwise       = n + sumNumbers (n - 1)

--- 6
sumSquares :: Int -> Int
sumSquares n
    | n == 0        = 0
    | otherwise     = n ^ 2 + sumSquares (n - 1)

--- 7
power :: Int -> Int -> Int
power n p
    | p == 0         = 1
    | otherwise      = n * power n (p - 1) 

power' :: Int -> Int -> Int
power' _ 0          = 1                     -- base case
power' n p          = n * power n (p - 1) 


--8
-- sumFromTo :: Int -> Int -> Int
-- sumFromTo num1 num2
--     | num2 < num1       = 0            -- base case if 2nd num is smaller
--     | num1 == num2      = num1         -- base case if equal
--     | otherwise         = num1 + sumFromTo (num1 + 1) num2

--9

gcd :: Int -> Int -> Int
gcd a b
  | a == b = a
  | a > b = gcd (a - b) b
  | otherwise = gcd (b - a) a







--10




