-- examples
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- Question 1
mult10 :: [Int] -> [Int]
mult10 xs = map (* 10) xs

-- Question 2
-- onlyLowerCase :: String -> String
-- onlyLowerCase
