minMaxCartao :: String -> (Double, Double)
minMaxCartao str = (menorQue (strToDouble (listaValores (separador str))), maiorQue (strToDouble (listaValores (separador str))))

separador :: String -> [String]
separador [] = []
separador (x:xs)
    | x == ';' = separador xs
    | otherwise = (takeWhile (/= ';') (x:xs)) : separador (dropWhile (/= ';') (x:xs))

listaValores :: [String] -> [String]
listaValores [] = []
listaValores lista = [lista !! (i) | i <- [0..length lista - 1], (i+1) `mod` 3 == 0]

strToDouble :: [String] -> [Double]
strToDouble [] = []
strToDouble (x:xs) = (read x :: Double) : strToDouble xs

maiorQue :: [Double] -> Double
maiorQue [] = 0
maiorQue (x:xs)
    | x > maiorQue xs = x
    | otherwise = maiorQue xs

menorQue :: [Double] -> Double
menorQue [x] = x
menorQue (x:xs)
    | x < menorQue xs = x
    | otherwise = menorQue xs

main = do
    a <- getLine
    let result = minMaxCartao a
    print result