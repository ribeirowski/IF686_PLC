logMes :: String -> String -> Double
logMes str [] = 0
logMes str lista = somador (strToDouble (ehJaneiro (casal (listaValores (separador lista))) str))

separador :: String -> [String]
separador [] = []
separador (x:xs)
    | x == ';' = separador xs
    | otherwise = (takeWhile (/= ';') (x:xs)) : separador (dropWhile (/= ';') (x:xs))

listaValores :: [String] -> [String]
listaValores [] = []
listaValores lista = [lista !! (i) | i <- [0..length lista - 1], (i+1) `mod` 3 == 0 || i `mod` 3 == 0]

casal :: [String] -> [(String, String)]
casal [] = []
casal (x:y:xs) = (x,y) : casal xs

ehJaneiro :: [(String, String)] -> String -> [String]
ehJaneiro [] str = []
ehJaneiro (x:xs) str
    | (fst x !! 3 == str !! 0) && (fst x !! 4 == str !! 1) && (fst x !! 5 == str !! 2) = snd x : ehJaneiro xs str
    | otherwise = ehJaneiro xs str

strToDouble :: [String] -> [Double]
strToDouble [] = []
strToDouble (x:xs) = (read x :: Double) : strToDouble xs

somador :: [Double] -> Double
somador [] = 0
somador (x:xs) = foldl (+) 0 (x:xs)

main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result