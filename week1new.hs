import Prelude

--worksheet 1

--1
timesTen :: Int -> Int
timesTen n = n * 10

--2
sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c

--3
areaOfCircle :: Float -> Float
areaOfCircle radius = pi * radius ^2

--4
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder height radius = areaOfCircle radius * height

--5
distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt((y1 - y2) ^2 + (x1 - x2) ^2)

--6
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && a /= c && b /= c

--7
divisibleBy :: Int -> Int -> Bool
divisibleBy a b = a `mod` b == 0

--8
isEven :: Int -> Bool
isEven num = divisibleBy num 2

--9
averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral(a + b + c) / 3

--10
absolute :: Int -> Int
absolute a = if a < 0 then (-a) else a

--worksheet 2
