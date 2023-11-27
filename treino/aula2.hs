--adicina n espacos a uma string
addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos (n-1)

--adiciona n espacos à esquerda de uma string
paraDireita :: Int -> String -> String
paraDireita n string = addEspacos n ++ string

vendas :: Int -> Int
vendas n
    | n == 0 = 10
    | n == 1 = 20
    | n == 2 = 30
    | n == 3 = 20
    | n == 4 = 10
    | otherwise = 0

totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = vendas n + totalVendas (n-1)

maxVendas :: Int -> Int
maxVendas 0 = vendas 0
maxVendas n = max (vendas n) (maxVendas (n-1))

cabecalho :: String
cabecalho = "Semana" ++ paraDireita 4 "Vendas\n"

media :: Int -> Float
media n = fromIntegral (totalVendas (n)) / fromIntegral (n+1)

imprimeSemanas :: Int -> String
imprimeSemanas 0 = paraDireita 2 (show 0) ++ paraDireita 8 (show(vendas 0))
imprimeSemanas n = imprimeSemanas (n-1) ++ "\n" ++ paraDireita 2 (show n) ++ paraDireita 8 (show(vendas n))

imprimeTotal :: Int -> String
imprimeTotal n = "Total" ++ paraDireita 6 (show(totalVendas n)) ++ "\n"

imprimeMedia :: Int -> String
imprimeMedia n = "Media" ++ paraDireita 6 (show(media n)) ++ "\n"

imprimidao :: Int -> IO()
imprimidao n 
    | n > 9 = putStr("Erro!" ++ "\n")
    | otherwise = putStr(cabecalho ++ imprimeSemanas n ++ "\n" ++ imprimeTotal n ++ imprimeMedia n ++ "\n")