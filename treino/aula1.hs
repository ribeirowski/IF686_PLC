--retorna a quantidade de vendas de uma semana n:
vendas :: Int -> Int
vendas n
    | n == 0 = 10
    | n == 1 = 20
    | n == 2 = 30
    | n == 3 = 20
    | n == 4 = 10
    | otherwise = 0

--retorna se um número é primo:
ePrimo :: Int -> Bool
ePrimo 0 = False
ePrimo 1 = False
--ePrimo n = length [x | x <- [2..n-1], mod n x == 0] == 0

--retorna se dois números são primos entre si:
primoEntreSi :: Int -> Int -> Bool
primoEntreSi x y

--retorna o fatorial de um número:
fatorial :: Int -> Int


--compara se quatro números são iguais:
all4Equal :: Int -> Int -> Int -> Int -> Bool


--retorna quantos parâmetros são iguais:
equalCount :: Int -> Int -> Int -> Int