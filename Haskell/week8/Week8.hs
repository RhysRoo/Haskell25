

helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do 
    putStr "Enter the filename: "
    name <- getLine
    contents <- readFile name
    putStr contents

getInt :: IO Int
getInt = do 
    str <- getLine
    return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
   | str == reverse str  = str ++ " is a palindrome"
   | otherwise           = str ++ " is not a palindrome"

pal :: IO ()
pal = do 
    line <- getLine
    let response = isPalindrome line
    putStrLn response

palLines :: IO ()
palLines = do 
    putStr "Enter a line: "
    str <- getLine
    if str == "" then 
        return ()
    else do 
        putStrLn (isPalindrome str)
        palLines

--- 1
greeting :: IO()
greeting = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn $ "Hello, " ++ name

---2
addTwoNumbers :: IO()
addTwoNumbers = do
    putStrLn "Enter first number"
    input1 <- getLine
    putStrLn "Enter second number"
    input2 <- getLine
    let number1 = read input1 :: Int    -- converts input to integer
    let number2 = read input2 :: Int
    let sum = number1 + number2
    putStrLn $ "The sum is " ++ show sum   -- show turns int to string before concatenating the 2 strings
    --print (number1 + number2)

---3
copyFile :: IO()
copyFile = do
    putStrLn "What is file name to copy"
    fileOne <- getLine
    putStrLn "What is the destination file name"
    fileTwo <- getLine

    -- read from file1
    text <- readFile fileOne
    putStrLn text
    --write (copy file1 content) to file2
    writeFile fileTwo text

--- 4
buildList :: [String] -> IO()
buildList list = do
    putStrLn "Enter a word (or exit):"
    word <- getLine
    if word == "exit"
        then print list
    else do
        let newList = list ++ [word]
        buildList newList

listBuilder :: IO()
listBuilder = buildList []


--- 5
sumInt :: Int -> IO Int
sumInt 0    = return 0
sumInt n   = do
    putStrLn "Enter number:"
    input <- getLine
    let number = read input :: Int    -- converts input to integer
    restOfNumbers <- sumInt (n-1)
    return (number + restOfNumbers)



main :: IO()
main = do
    putStrLn "How many numbers would you like to add up?"
    input <- getLine
    let n = read input :: Int    -- converts input to integer
    putStrLn "Enter numbers one by one:"
    sum <- sumInt n
    putStrLn $ "sum is " ++ show sum