-- Dado uma lista de números inteiros, retorne uma lista de somas de intervalos desses números, tais que cada soma retornada é de números que estão separados por 0's, caso dois zeros lidos em sequência retorne a lista atualmente computada

maquinaSomar :: [Int] -> [Int]
maquinaSomar [] = []
maquinaSomar lista = somador (separador lista)

separador :: [Int] -> [[Int]]
separador [] = []
separador (0:0:xs) = []
separador (0:xs)
    | xs /= [] = separador xs
    | otherwise = []
separador lista = (takeWhile (/= 0) lista) : separador (dropWhile (/= 0) lista)

somador :: [[Int]] -> [Int]
somador [] = []
somador (x:xs) = sum x : somador xs

main = do
    lista <- getLine
    print $ maquinaSomar (read lista :: [Int])