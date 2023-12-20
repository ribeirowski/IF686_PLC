main = do
    sentence_1 <- getLine
    sentence_2 <- getLine
    let result = uncommonFromTwoSentences sentence_1 sentence_2
    print result

uncommonFromTwoSentences :: String -> String -> [String]
uncommonFromTwoSentences string1 string2 = removeSpace(repeticao(ordemAlfabetica(diferentes (separador (minusculo(string1))) (comparador (separador (minusculo(string1))) (separador (minusculo(string2)))) ++ diferentes (separador (minusculo(string2))) (comparador (separador (minusculo(string2))) (separador (minusculo(string1)))))))

separador :: String -> [String]
separador [] = []
separador string = takeWhile (/= ' ') string : separador (drop (length (takeWhile (/= ' ') string) + 1) string)

comparador :: [String] -> [String] -> [Bool]
comparador [] _ = []
comparador (x:xs) lista
    | x `elem` lista = True : comparador xs lista
    | otherwise = False : comparador xs lista

diferentes :: [String] -> [Bool] -> [String]
diferentes [] _ = []
diferentes (x:xs) (y:ys)
    | y == True = diferentes xs ys
    | otherwise = x : diferentes xs ys

ordemAlfabetica :: [String] -> [String]
ordemAlfabetica [] = []
ordemAlfabetica (x:xs) = ordemAlfabetica [y | y <- xs, y < x] ++ [x] ++ ordemAlfabetica [y | y <- xs, y >= x]

minusculo :: String -> String
minusculo [] = []
minusculo (x:xs)
    | x >= 'A' && x <= 'Z' = toEnum (fromEnum x + 32) : minusculo xs
    | otherwise = x : minusculo xs

repeticao :: [String] -> [String]
repeticao [] = []
repeticao (x:xs)
    | x `elem` xs = repeticao xs
    | otherwise = x : repeticao xs

removeSpace :: [String] -> [String]
removeSpace [] = []
removeSpace (x:xs)
    | x == "" = removeSpace xs
    | otherwise = x : removeSpace xs