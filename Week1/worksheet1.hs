import Prelude

--Q1
timesTen :: Int -> Int
timesTen x = x * 10

--Q2
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

--Q3
areaOfCircle :: Float -> Float
areaOfCircle r = pi * r ^ 2

--Q4
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder h r = h * areaOfCircle r

--Q5
distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt((y1 - y2)^2 + (x1 - x2)^2)

--Q6
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && y /= z && x /= z

--Q7
divisibleBy :: Int -> Int -> Bool
divisibleBy x y = mod x y == 0
-- divisibleBy x y = x `mod` y == 0 
    
--Q8 (x mod 2 == 0 is even and x mod 2 == 1 is odd)
isEven :: Int -> Bool
isEven x = divisibleBy x 2

--Q9
averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

--applyDiscount :: Float -> Int -> Float
--applyDiscount price percent = price * (1 - fromIntegral percent / 100)

--Q10
absolute :: Int -> Int
absolute num = if num < 0 then -num else num      -- num * (-1)