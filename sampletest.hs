-- M21274 MATHFUN Functional Programming in-class test 2024/25 - Sample


{-
********
Without using AI, only relying on my own past code from this year's worksheets:

- Solve exercise 1 both ways with pattern matching and guards [X]

- Exercise 2 (list comprehension) [X]

- Exercise 3 recursion (week 5)

*******
-}



{-
Exercise 1 - 4 marks
--------------------
Using guards or pattern matching, write a price function which returns the
price of an item on a menu, where "Pizza" costs 8.25, "Pasta" costs 7.95, and
"Salad" costs 6.55. Your function should return 0.0 for any other parameter
value (i.e. everything else on the menu is free).
-}

price :: String -> Float -- Pattern Matching
price "Pizza"         = 8.25
price "Pasta"         = 7.95
price "Salad"         = 6.55
price _               = 0.0

price' :: String -> Float -- Guards
price' item                             -- price(item)
  | item == "Pizza"   = 8.25
  | item == "Pasta"   = 7.95
  | item == "Salad"   = 6.55
  | otherwise         = 0.0


--1B

course :: String -> String
course "Sam"              = "Computing"
course "Bob"              = "Cyber Security"
course "Amy"              = "Physics"
course _                  = "Unknown"

course' :: String -> String
course' name
    | name == "Sam"             = "Computing"
    | name == "Bob"             = "Cyber Security"
    | name == "Amy"             = "Physics"
    | otherwise                 = "Unknown"



addToAll :: [Int] -> [Int]
addToAll lst                = map (+5) lst

gradeLetter :: Int -> Char
gradeLetter grade
    | grade < 0 || grade > 100          = error "out of range"
    | grade >= 70                       = 'A'
    | grade >= 60                       = 'B'
    | grade >= 50                       = 'C'
    | grade >= 40                       = 'D'
    | otherwise                         = 'F'




{-
Exercise 2 - 4 marks
--------------------
Using a list comprehension and the describeTemperature function below, write a
describePlaceTemperatures function that takes a list of place-temperature pairs
(like the testPlaces list), and returns a list of place-descriptions pairs.
Running your function on testPlaces should give:
  [("London","Cold"),("Madrid","Hot"),("Paris","Warm"),("Helsinki","Freezing")]
-}

testPlaces :: [(String, Int)]
testPlaces = [("London", 12), ("Madrid", 32), ("Paris", 22), ("Helsinki", -3)]

describeTemperature :: Int -> String
describeTemperature temp
   | temp >= 30   = "Hot"
   | temp >= 15   = "Warm"
   | temp >= 1    = "Cold"
   | otherwise    = "Freezing"

describePlaceTemperatures :: [(String, Int)] -> [(String, String)]
describePlaceTemperatures testPlaces' = [ (location, describeTemperature temp) | (location, temp) <- testPlaces']


{-     EXPLANATION:
Tha above function uses a list comprehension that iterates over each (place, temp) pair in the input list places.
For each pair, it applies describeTemperature to temp to get the corresponding temperature description
and pairs it back with place.

1. describePlaceTemperatures places = ...:
This part defines a function describePlaceTemperatures that takes one argument, places.
places is expected to be a list of pairs, where each pair contains a String (representing the place)
and an Int (representing the temperature).

2. [(location, describeTemperature temp) | ... ]:
This part is the list comprehension itself. The expression before the | symbol specifies the format of the elements in the new list being created.
Each element is a tuple:
location is the name of a place, retained directly from the input pairs.
describeTemperature temp calls the function describeTemperature on temp to get a descriptive string based on the temperature value.

3.(location, temp) <- places:
This part is a generator. It iterates over each element of the places list. For each element (which is expected to be a tuple (location, temp)),
it "unpacks" or "binds" the values into the variables location and temp.
location is used directly in the output tuple, and temp is used as the argument to the describeTemperature function.
-}

-- 2B

testScores :: [(String, Int)]
testScores = [("Alice", 92), ("Bob", 38), ("Charlie", 76), ("Dana", 55)]

classifyScore :: Int -> String
classifyScore score
   | score >= 90  = "Excellent"
   | score >= 70  = "Good"
   | score >= 50  = "Average"
   | otherwise    = "Failing"

describeStudentPerformance :: [(String, Int)] -> [(String, String)]
describeStudentPerformance scoresList = [(name, classifyScore score) | (name, score) <- scoresList]

--2C

data Vehicle = Car | Bike | Truck | Scooter
  deriving (Show)

vehicleData :: [(Vehicle, Float)]
vehicleData = [(Car, 120.5), (Bike, 25.0), (Truck, 80.0), (Scooter, 45.5)]

rateSpeed :: Float -> String
rateSpeed speed
   | speed >= 100 = "Fast"
   | speed >= 60  = "Moderate"
   | speed >= 30  = "Slow"
   | otherwise    = "Very Slow"

describeVehicleSpeeds :: [(Vehicle, Float)] -> [(Vehicle, String)]
describeVehicleSpeeds vehicleDataList = [(vehicle, rateSpeed speed) | (vehicle , speed ) <- vehicleDataList]


-- 2D

{-
Using a list comprehension and the evaluateScore function below, write a
gradeSubmissions function that takes a list of submission-result triples (like the submissionData list),
and returns a list of submission-feedback pairs.

Running your function on submissionData should give something like:
[(("John",True),"Pass"), (("Emma",False),"Fail"), ...]
-}

submissionData :: [(String, Bool, Int)]
submissionData = [("John", True, 90), ("Emma", False, 45), ("Liam", True, 85), ("Ava", True, 30), ("Rhys", True, 91), ("Eleni", False, 92)]

evaluateScore :: Int -> Bool -> String
evaluateScore score onTime
   | not onTime     = "Capped at 40"
   | score >= 80    = "Distinction"
   | score >= 40    = "Pass"
   | otherwise      = "Fail"

gradeSubmissions :: [(String, Bool, Int)] -> [((String, Bool), String)]
gradeSubmissions submissionDataList = [((name, onTime), evaluateScore score onTime) | (name, onTime, score) <- submissionDataList, score >= 90 && onTime]
-- gradeSubmissions submissionDataList = [((name, onTime), evaluateScore score onTime) | (name, onTime, score) <- submissionDataList, score < 40 || not onTime]

--2E

bookRatings :: [(String, Double)]
bookRatings = [("The Silent Patient", 4.2), ("Old Man and the Sea", 3.8), ("Dune", 4.6), ("1984", 4.0)]

categoriseBook :: Double -> String
categoriseBook rating
   | rating >= 4.5 = "Must Read"
   | rating >= 4.0 = "Recommended"
   | otherwise     = "Optional"

listPopularBooks :: [(String, Double)] -> [(String, String)]
listPopularBooks bookRatingList = [(title, categoriseBook rating) | (title, rating) <- bookRatingList, rating >= 4.0]

--2F

attendanceData :: [(String, Int)]
attendanceData = [("Alice", 95), ("Bob", 68), ("Charlie", 82), ("Dana", 40)]

classifyAttendance :: Int -> String
classifyAttendance percent
   | percent >= 90 = "Excellent"
   | percent >= 75 = "Good"
   | percent >= 70 = "Satisfactory"
   | otherwise     = "Insufficient"

reportRegularStudents :: [(String, Int)] -> [(String, String)]
reportRegularStudents attendanceDataList = [(name, classifyAttendance attendance) | (name, attendance) <- attendanceDataList, attendance > 70]

--2G

studentGrades :: [(String, Int)]
studentGrades = [("Alice", 95), ("Bob", 35), ("Charlie", 82), ("Dana", 39)]

failedStudents :: [(String, Int)] -> [String]
failedStudents studentGradesList = [name | (name , grade) <- studentGradesList, grade < 40]

-- higher order functions / composition

squareOfNums :: [Int] -> [Int]
squareOfNums lst = map (^2) lst

-- Only square the numbers below 10
squareOfNumsLowerThan10 :: [Int] -> [Int]
-- squareOfNumsLowerThan10 lst = (map (^2) . filter (<10)) lst
squareOfNumsLowerThan10 = map (^2) . filter (<10)


squareOfOdd :: [Int] -> [Int]
squareOfOdd = map (*2) . filter odd

addNumsExcept10and20 :: [Int] -> [Int]
addNumsExcept10and20 = map (*2) . filter (\x -> x/= 10 && x/=20)


-- using filter with 2 conditions
-- filter (\x -> x > 3 && x < 10 ) [1,3,5,2,12,15,20]



{-
Exercise 3 - 4 marks
--------------------
Using recursion, write a hotter function that takes a temperature and a list
of place-temperature pairs (like the testPlaces list) and returns a list of
places from the list that are hotter than the given temperature.
-}

-- testPlaces :: [(String, Int)]
-- testPlaces = [("London", 12), ("Madrid", 32), ("Paris", 22), ("Helsinki", -3)]

-- hotter :: Int -> [(String, Int)] -> [String]
-- hotter _ []                                     = []
-- hotter n ((place, temp):xs)
--    | n < temp                                   = place : hotter n xs
--    | otherwise                                  = hotter n x

-- 3B Create a function that checks places that are colder then the given temperatures

colderThan :: Int -> [(String, Int)] -> [String]
colderThan _ []                                 = []
colderThan n ((place, temp): xs)
    | temp < n                                     = place : colderThan n xs
    | otherwise                                    = colderThan n xs

-- 3C Create a function that gives places between the upper and lower bound temperatures

withinRange :: Int -> Int -> [(String, Int)] -> [String]
withinRange _ _ []                                 = []
withinRange low up ((place, temp): xs)
    | temp > low && temp < up                     = place : withinRange low up xs
    | otherwise                                    = withinRange low up xs

-- 3D

testBooks :: [(String, Int)]
testBooks =
  [ ("The Hobbit",           310)
  , ("War and Peace",       1225)
  , ("The Little Prince",     96)
  , ("Moby Dick",            635)
  , ("1984",                 328)
  , ("Pride and Prejudice",  432)
  , ("Animal Farm",           112)
  ]

-- Gives the books that have pages that are less than n

shorterThan :: Int -> [(String, Int)] -> [String]
shorterThan n []                            = []
shorterThan n ((bookTitle, pages): xs)
    | pages < n                             = bookTitle : shorterThan n xs
    | otherwise                             = shorterThan n xs

-- 3E


{-
Exercise 4 - 3 marks
--------------------
The List type definition below represents lists of integers, and the testList
value is an example of an List whose values are in order. Write an insert
function that takes an int and a List that is ordered, and inserts the integer
into the List so that it remains ordered.
-}


data List = Null | Node Int List
     deriving (Show)

testList :: List
testList = Node 2 (Node 3 (Node 5 (Node 5 (Node 6 Null))))

insert :: Int -> List -> List
insert n Null                                        = Node n Null
insert n (Node nodeVal xs)
    | n <= nodeVal                                   = Node n (Node nodeVal xs)
    | otherwise                                      = Node nodeVal (insert n xs)


--4B
{-Write a remove function that takes an Int and a List, and removes the first
occurrence of the given integer from the list. If the integer is not found,
return the list unchanged.
-}

-- data List = Null | Node Int List
--     deriving (Show)

testList' :: List
testList' = Node 2 (Node 3 (Node 5 (Node 5 (Node 6 Null))))

remove :: Int -> List -> List
remove _ Null                                       = Null
remove n (Node nodeVal xs)
    | n == nodeVal                                  = xs
    | otherwise                                     = Node nodeVal (remove n xs)


-- 4C
listLength :: List -> Int
listLength Null                                     = 0
listLength (Node nodeVal xs)
    | nodeVal > 0                                   = 1 + listLength xs
    | otherwise                                     = listLength xs

-- 4D
-- reverseList :: List -> List
-- reverseList Null                                    = Null
-- reverseList (Node nodeVal xs)
--     | nodeVal < xs                             = Node nodeVal : reverseList xs
--     | otherwise                                = reverseList xs


-- 4E
contains :: Int -> List -> Bool
contains n Null                                 = False
contains n (Node nodeVal xs)
    | n == nodeVal                               = True
    | otherwise                                 = contains n xs

-- 4D
testList'' :: List
testList'' = Node 2 (Node 3 (Node 5 (Node 5 (Node 2(Node 6 Null)))))

removeAll :: Int -> List -> List
removeAll n Null                                = Null
removeAll n (Node nodeVal xs)
    | n /= nodeVal                              = Node nodeVal (removeAll n xs)
    | otherwise                                 = removeAll n xs



-- 4E - Builds a list from a array
fromList :: [Int] -> List
fromList []                                     = Null
fromList (x:xs)
    | length xs > 0                             = Node x (fromList xs)
    | otherwise                                 = Node x Null

{-

    -- (Trees) Write a insert2 function that writes a new node that takes an integer and
    -- a binary tree that is ordered (BST) and inserts the node into the tree so that it
    -- remains ordered



data Tree = Null | Node Int Tree Tree
    deriving (Show)

exampleBST = Node 8
            (Node 3
                (Node 1 Null Null)
                (Node 6
                    (Node 4 Null Null)
                    (Node 7 Null Null)
                )
            )
            (Node 10
                Null
                (Node 14
                    (Node 13 Null Null)
                    Null
                )
            )

insert2 :: Int -> Tree -> Tree
insert2 n Null                      = Node n Null Null
insert2 n (Node v l r)
    | n < v                         = Node v (insert2 n l) r
    | otherwise                     = Node v l (insert2 n r)

-}

{-
Test function. Use this function to test your solutions. You should uncomment
all lines relating to the exercises you have attempted, but should not change
anything else.

To test the functionality of your code we will only run this function so
make sure that:
  * you uncommented all appropriate lines in the function
  * you comment out any incomplete/non-working solutions
-}

test :: IO ()
test = do
   putStrLn "Exercise 1"
   putStrLn $ "  Pizza - " ++ show (price "Pizza")
   putStrLn $ "  Pasta - " ++ show (price "Pasta")
   putStrLn $ "  Salad - " ++ show (price "Salad")
   putStrLn $ "  Fish - " ++ show (price "Fish")
   putStrLn "Using guards:"
   putStrLn $ "  Pizza - " ++ show (price' "Pizza")
   putStrLn $ "  Pasta - " ++ show (price' "Pasta")
   putStrLn $ "  Salad - " ++ show (price' "Salad")
   putStrLn $ "  Fish - " ++ show (price' "Fish")
   putStrLn "Exercise 2"
   putStrLn $ "  " ++ show (describePlaceTemperatures testPlaces)
   -- putStrLn "Exercise 3"
   -- putStrLn $ "  " ++ show (hotter 12 testPlaces)
   -- putStrLn "Exercise 4"
   -- putStrLn $ "  Inserting 1 gives - " ++ show (insert 1 testList)
   -- putStrLn $ "  Inserting 3 gives - " ++ show (insert 3 testList)
   -- putStrLn $ "  Inserting 4 gives - " ++ show (insert 4 testList)
