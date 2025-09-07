nor :: Bool -> Bool -> Bool
nor False False = True
nor False True = False
nor True False = False
nor True True = False

-- fibonacci :: Int -> Int
-- fibonacci 0 = 0 -- base case 
-- fibonacci 1 = 1
-- fibonacci x = fibinacci (x-1) + fibinacci (x-2)

fibonacci' :: Int -> Int
-- fibonacci' n 
    -- | n == 0 = 0
    -- | n == 1 = 1
    -- | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
fibonacci' 0 = 0 -- base case 
fibonacci' 1 = 1
fibonacci' n = fibonacci'(n - 1) + fibonacci' (n - 2)
