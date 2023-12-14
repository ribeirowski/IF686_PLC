main = do
    input <- getLine
    let result = palmeirinha input
    print result

seqFibonacci :: Int -> Int
seqFibonacci 0 = 0
seqFibonacci 1 = 0
seqFibonacci 2 = 1
seqFibonacci n = seqFibonacci (n-1) + seqFibonacci (n-2)

listaFibonacci :: Int -> [Int]
listaFibonacci 2 = [0,1,1]
listaFibonacci n = listaFibonacci (n-1) ++ [seqFibonacci n]

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) = [x | x <- (x:xs), x `mod` 2 == 0]

soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

palmeirinha :: String -> String
palmeirinha [] = []
palmeirinha str = str ++ show (soma (pares (listaFibonacci (length str))))