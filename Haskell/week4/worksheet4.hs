import Data.Char
import Prelude

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1, m1) (s2, m2)
  | m1 >= m2 = s1
  | otherwise = s2

marks :: [StudentMark] -> [Int]
marks stmks = [mk | (st, mk) <- stmks]

pass :: [StudentMark] -> [String]
pass stmks = [st | (st, mk) <- stmks, mk >= 40]

-- An example list of student marks
testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]

addPairs :: [(Int, Int)] -> [Int]
addPairs pairList = [i + j | (i, j) <- pairList]

minAndMax :: Int -> Int -> (Int, Int)
minAndMax x y
  | x <= y = (x, y)
  | otherwise = (y, x)

-- recursion demo
fac :: Int -> Int
fac n
  | n <= 1 = 1
  | otherwise = n * fac (n - 1)

-- patten matching
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

-- \| x <= y = (x + y, y - 1)
-- \| otherwise = (y + 2, x - 7)

grade :: StudentMark -> Char
grade (name, mark)
  | mark >= 70 = 'A'
  | mark >= 60 = 'B'
  | mark >= 50 = 'C'
  | mark >= 40 = 'D'
  | otherwise = 'F'

lowerOrUpperCase :: Char -> String
lowerOrUpperCase c
  | isLower c = "lower case"
  | isUpper c = "upper case"
  | otherwise = error "Not a letter"

capMark :: StudentMark -> StudentMark
capMark (name, mark)
  | mark > 40 = (name, 40)
  | otherwise = (name, mark)

firstNumbers :: Int -> [Int]
firstNumbers n = [1 .. n]

firstSquares :: Int -> [Int]
firstSquares n = [i ^ 2 | i <- firstNumbers n]

capitalise :: String -> String
capitalise str = [toUpper c | c <- str]

onlyDigits :: String -> String
onlyDigits msg = [c | c <- msg, isDigit c]

-- capMarks :: [StudentMark] -> [StudentMark]
-- capMarks

incrementAll :: [Int] -> [Int]
incrementAll lst = [i + 1 | i <- lst]
