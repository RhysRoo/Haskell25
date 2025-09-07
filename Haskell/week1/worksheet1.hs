timesTen :: Int -> Int
timesTen x = x * 10

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

areaOfCircle :: Float -> Float
areaOfCircle r = pi * r ^ 2

volumeOfCylinder :: Float -> Float -> Float
-- volumeOfCylinder r h = areaOfCircle r * h
volumeOfCylinder r h = areaOfCircle r * h

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((y1 - y2) ^ 2 + (x1 - x2) ^ 2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && a /= c && b /= c

divisibleBy :: Int -> Int -> Bool
divisibleBy a b = a `mod` b == 0
-- divisibleBy a b = mod a b == 0



isEven :: Int -> Bool
isEven n = divisibleBy n 2

averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral(a + b + c) / 3

--absolute :: Int -> Int

