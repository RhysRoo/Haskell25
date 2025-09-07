import GHC.IO.Handle (NewlineMode(outputNL))
-- intro

heartMonitor :: Int -> Int -> String
heartMonitor age bpm
    | age > 80 && bpm > 100 = "High heart rate for 81+!"
    | age > 60 &&  bpm > 130 = "High heart rate for 61-80!"
    | age > 40 &&  bpm > 140 = "High heart rate for 41-60!"
    | age > 20 &&  bpm > 155 = "High heart rate for 21-40!"
    | age >= 0 && bpm > 170 = "High heart rate for 0-20!" 
    | otherwise = "Normal heart rate"

-- exercises

--1)



--2) 
sign :: Int -> Int
sign x
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0

-- 3)
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | a == b && b == c = 3
    | a == b || a == c || b == c = 2
    | otherwise = 0


--4
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths l1 l2 l3 = sqrt (2 * (l1 + l2 + l3)^2)

--5
taxiFare :: Int -> Float
taxiFare km
    | km <= 10 = (fromIntegral km * 0.5) + 2.20
    | km >= 11 = (fromIntegral km * 0.3) + 2.20

--6 
-- howManyAboveAverage :: Int -> Int -> Int -> Int
-- howManyAboveAverage 

--7

validDate :: Int -> Int -> Bool
validDate day month
    | day > 0 && day <= 31 && month > 0 && month <= 12 = True
    | otherwise = False


daysInMonth :: Int -> Int -> Int
daysInMonth month year
  | month == 2 && isLeapYear = 29
  | month == 2 = 28
  | month `elem` [4, 6, 9, 11] = 30
  | otherwise = 31
  where
    isLeapYear = year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)


{-

Q1a) sumThree 3 5 7

step 0 - The function will scan through the fuction looking for any partenthies as the instruction within this
must be run first. if none are found in this case it will begin at the start again.

step 1 - 3 is seen as its the first visable input of the function

step 2 - after 3 has been seen. the function will run the next instruction which is the addition operator

step 3 - after the addition operator the function will look at what to value to add to this operator 
which is 5 so 3 + 5 will be added together and returned as the provisional result, but will keep going until
the code has cleared. 

step 4 - another addition operator

step 5 - result (3 + 5  = 8) + the next value which is 7

step 6 = 8 + 7 will be executed which returns a provisional reslut of 15

step 7 - are there anymore instrustructs? no

step 8 - return the provisional result as output

Q1b) sumThree 8 (1 + 3) 2
step 1 - The function will scan through the lines of inputs in a linear way starting from value 1 until the ending, 
but won't start until it knows there are no partenthies. in this line of code partenthies are present so looks inside and checks
if its the only partenthies within the partenthies.

Step 2 inside the partenthies now it will look for operators and in this case 1 + 3 will be added together
which equals 4 

Step 3)now the partenthies has been complete, the program will start at the very start of the programs inputs, but
instead of 8 (1 + 3) 2, the program will see 8 + 4 + 2

Step 4) 8 + 4 + 2 = 14

Q2a)


-}