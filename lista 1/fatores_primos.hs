listaPrimo :: Int -> [Int]
listaPrimo 0 = []
listaPrimo 1 = []
listaPrimo 2 = [2]
listaPrimo n = [x | x <- [2..(n-1)]]

decomposicao :: [Int] -> Int -> [Int]
decomposicao [] 0 = []
decomposicao (x:xs) n
    | n `mod` x == 0 = x : decomposicao (x:xs) (n `div` x)
    | xs == [] = []
    | otherwise = decomposicao xs n

decompoe :: Int -> [Int]
decompoe 0 = []
decompoe 1 = []
decompoe n = decomposicao (listaPrimo n) n

repeticao :: [Int] -> [(Int, Int)]
repeticao [] = []
repeticao (x:xs) = (x, length [y | y <- (x:xs), y == x]) : repeticao [y | y <- (x:xs), y /= x]

fatPrime :: Int -> [(Int, Int)]
fatPrime n = repeticao (decompoe n)

main = do
    a <- getLine
    let result = fatPrime (read a :: Int)
    print result