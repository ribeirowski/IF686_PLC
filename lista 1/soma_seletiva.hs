sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

numList :: String -> [Int]
numList [] = []
numList (x:xs)
| x >= '0' && x <= '9' = (fromEnum x - fromEnum '0') : numList xs
| otherwise = numList xs

sumNumbers :: String -> Int
sumNumbers str = sumList (numList str)

main = do
    a <- getLine
    let result = sumNumbers a
    print result