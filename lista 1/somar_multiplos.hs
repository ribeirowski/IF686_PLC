somarMultiplos :: [Int] -> Int -> [Int]
somarMultiplos [] num = []
somarMultiplos (x:xs) num
    | x >= num = somador(multiplos (elemento x) num) : somarMultiplos xs num
    | x < num || num == 0 = 0 : somarMultiplos xs num
    | otherwise = somarMultiplos xs num

elemento :: Int -> [Int]
elemento 0 = []
elemento num = num : elemento (num-1)

multiplos :: [Int] -> Int -> [Int]
multiplos [] num = []
multiplos (x:xs) num
    | num /= 0 && x `mod` num == 0 = x : multiplos xs num
    | otherwise = multiplos xs num

somador :: [Int] -> Int
somador [] = 0
somador (x:xs) = x + somador xs

main = do
    lista <- getLine
    let readList = read lista :: [Int]
    num <- getLine
    let readNum = read num :: Int
    let result = somarMultiplos readList readNum
    print result