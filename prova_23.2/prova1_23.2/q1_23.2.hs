fibonacci :: [Int]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

fibonacciTeste :: Int -> [Int]
fibonacciTeste n = take n fibonacci
