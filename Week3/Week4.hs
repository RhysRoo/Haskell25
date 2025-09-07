import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2) 
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y 
    | x <= y            = (x,y)
    | otherwise         = (y,x)

--Q1
sumDifference :: Int -> Int -> (Int,Int)
sumDifference a b = (a + b, a - b)

--Q2
grade :: StudentMark -> Char
grade (sName, mark)
    | mark > 100 || mark < 0        = error "Not a letter"
    | mark >= 70                    = 'A'
    | mark >= 60                    = 'B'
    | mark >= 50                    = 'C'
    | mark >= 40                    = 'D'
    | otherwise                     = 'F'

--Q3
capMark :: StudentMark -> StudentMark
capMark (name, mark)
  | mark > 40 = (name, 40)
  | otherwise = (name, mark)

--Q4
firstNumbers :: Int -> [Int]
firstNumbers targetNumber = [1 .. targetNumber]

--Q5
firstSquares :: Int -> [Int]
firstSquares targetNumberN = [x ^ 2 | x <- [1 .. targetNumberN]]

-- testList = [x * 3 | x <- [1 .. 5]]
testList :: Int -> [Int]
testList targetNumberN = [x * 3 | x <- [1 .. targetNumberN ]]

--Q6
capitalise :: String -> String
capitalise strs = [toUpper x | x <- strs]

--Q7
onlyDigits :: String -> String
onlyDigits s = [x | x <- s, isDigit x]

--Q8


--Q9
gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents student = [(sName, grade (sName, mark)) | (sName, mark) <- testData]

-- Q10
-- duplicate :: String -> Int -> String
-- duplicate str num
--    | num == 0 = ""
--    | otherwise = str ++ duplicate str (num - 1)

duplicate' :: String -> Int -> String
-- duplicate' str amount = [ 'h' | n <- [1..amount] ]      

-- duplicate' str amount = concat["la" | n <- [1..amount] ] 
duplicate' str amount = concat[str | n <- [1..amount] ]      


--Q11
divisors :: Int -> [Int]
-- divisors n = [x | x <- [0 .. n], n `mod` 2 == 0]
divisors n = [x | x <- [1 .. n], n `mod` x == 0]

--Q12
isPrime :: Int -> Bool
isPrime x = length (divisors x) == 2

--Q13



