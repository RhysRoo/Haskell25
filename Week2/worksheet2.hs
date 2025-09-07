--1
myAbs :: Int -> Int
myAbs x
    | x < 0 = -x
    | otherwise = x

--2
sign :: Int -> Int
sign x
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0

--3
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual numOne numTwo numThree
    | numOne == numTwo && numOne == numThree                            = 3
    | numOne == numTwo || numOne == numThree || numTwo == numThree      = 2
    | otherwise                                                         = 0 

--4
sideLength :: Float -> Float
sideLength x = x ^ 2


-- Need to use where to avoid repetition
sumDiagonalLength :: Float -> Float -> Float -> Float
sumDiagonalLength x y z = sqrt(2 * sideLengthX) + sqrt(2 * sideLengthY) + sqrt(2 * sideLengthZ)
    where 
       sideLengthX = x ^ 2 
       sideLengthY = y ^ 2
       sideLengthZ = z ^ 2 
 


-- sqrt(2 * sideLength^2) =  sqrt 2 * sqrt sideLength^2 = sqrt 2 * sideLength



sumDiagonalLength2 :: Float -> Float -> Float -> Float
sumDiagonalLength2 x y z = dlength1 + dlength2 + dlength3
    where 
        squareOf2 = sqrt 2
        dlength1 = squareOf2 * x
        dlength2 = squareOf2 * y
        dlength3 = squareOf2 * z

       

-- --5
taxiFare :: Int -> Float
taxiFare distanceKm
    | distanceKm < baseKmThreshold      = baseFare + (fromIntegral distanceKm * perKmBefore)
    | distanceKm >= baseKmThreshold     = baseFare + (fromIntegral baseKmThreshold * perKmBefore) + (fromIntegral (distanceKm - baseKmThreshold) * perKmAfter)
    | otherwise                         = 0
        where
            baseFare        = 2.20
            baseKmThreshold = 10
            perKmBefore     = 0.50
            perKmAfter      = 0.30

--6
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c = countAbove a + countAbove b + countAbove c
    where 
        average = fromIntegral (a + b + c) / 3
        countAbove x =  if fromIntegral x > average 
                        then 1 
                        else 0




--7
validDate :: Int -> Int -> Bool
validDate d m
    | m < 1 || m > 12                           = False
    | m == 2                                    = d >= 1 && d <= 28
    | m == 4 || m == 6 || m == 9 || m == 11     = d >= 1 && d <= 30
    | otherwise                                 = d >= 1 && d <= 31       



--8
daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | year `mod` 4 == 0 && month == 2                                   = 29
    | year `mod` 4 == 1 && month == 2                                   = 28
    | month == 4 || month == 6 || month == 9 || month == 11             = 30
    | otherwise                                                         = 31  


{--
1.
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

1a)
sumThree 3 5 7
~~> 3 + 5 + 7   (substituting)
~~> 8 + 7       (arithmetic 3 + 5 = 8)
~~> 15          (arithmetic 8 + 7 = 15)

1b)
sumThree 8 (1 + 3) 2
sumThree 8 4 2    (athrithmetic 1 + 3 = 4)
~~> 8 + 4 + 2     (substituting)
~~> 12 + 2        (athrithmetic 8 + 4 = 12)
~~> 14            (athrithmetic 12 + 2 = 14)    

2.
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && y /= z && x /= z

2a)
threeDifferent 1 4 2
1 /= 4 && 4 /= 2 && 1 /= 2      (Subsitution)
True && True && True            (Arithmetic all values are different)
True && True                    (Boolean arithmetic T and T = T)
True                            (Boolean arithmetic T and T = T)

2b)
threeDifferent 1 7 7
1 /= 7 && 7 /= 7 && 1 /= 7      (Subsitution)
True && False && True           (Arithmetic: 7 appears twice to make a False)
False && True                   (Boolean arithmetic T and F = False)
False                           (Boolean arithmetic F and T = False)
--}