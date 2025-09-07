import Prelude hiding (gcd, (&&), (||))

infixr 3 &&

-- Example

(||) :: Bool -> Bool -> Bool
True || True = True
False || True = True
True || False = True
False || False = False

-- Worked example
nor :: Bool -> Bool -> Bool
nor False False = True
nor False True = False
nor True False = False
nor True True = False

nor2 :: Bool -> Bool -> Bool
nor2 False x = not x
nor2 True _ = False

-- Question 1
(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

-- Question 2
exOr :: Bool -> Bool -> Bool
-- exOr False False = False
-- exOr False True  = True
exOr False a = a
-- exOr True  False = True
-- exOr True  True  = False
exOr True x = not x

-- Question 3
ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True a _ = a
ifThenElse _ _ b = b

-- Question 4
daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth _ = 31

-- Question 5
sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

-- Question 6
sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n ^ 2 + sumSquares (n - 1)

-- Question 7
power :: Int -> Int -> Int
power _ 0 = 1
power base exponent = base * power base (exponent - 1)

-- Question 8
sumFromTo :: Int -> Int -> Int
sumFromTo start end
  | start > end = 0
  | otherwise = start + sumFromTo (start + 1) end

-- Question 9
gcd :: Int -> Int -> Int
gcd a b
  | a == b = a
  | a > b = gcd (a - b) b
  | otherwise = gcd (b - a) a

-- Question 10

-- Question 11
