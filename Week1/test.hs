main :: IO ()
main = putStrLn "Hello, Haskell!"

square :: Int -> Int
square n = n * n

mult2 :: Int -> Int
mult2 x = 2 * x

mult4 :: Int -> Int
mult4 x = mult2 (mult2 x)

-- These two functions show how to multiply a number by 2 or by 4. The first function, mult2, takes a number and multiplies it by 2. The second function, mult4, reuses mult2 to effectively multiply the number by 4 by calling mult2 twice.
