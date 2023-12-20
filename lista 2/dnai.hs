main :: IO ()
main = do
    input <- getLine
    let result = dna1 (read input :: Tree Int)
    print result

data Tree t = Node t (Tree t) (Tree t) | Nilt
                deriving (Read, Show)

dna1 :: Tree Int -> [String]
dna1 Nilt = []
dna1 (Node x left right) = convertao (lista8 (mod5 (listaFull (Node x left right))))

converte :: [Int] -> String
converte [] = []
converte (x:xs)
    | x == 0 = 'E' : converte xs
    | x == 1 = 'M' : converte xs
    | x == 2 = 'A' : converte xs
    | x == 3 = 'C' : converte xs
    | x == 4 = 'S' : converte xs

convertao :: [[Int]] -> [String]
convertao [] = []
convertao (x:xs) = converte x : convertao xs

lista8 :: [Int] -> [[Int]]
lista8 [] = []
lista8 lista = [x | x <- take 8 lista] : lista8 (drop 8 lista)

mod5 :: [Int] -> [Int]
mod5 [] = []
mod5 (x:xs) = x `mod` 5 : mod5 xs

listaFull :: Tree Int -> [Int]
listaFull Nilt = []
listaFull (Node x Nilt Nilt) = [x]
listaFull (Node x left right) = listaFull left ++ [x] ++ listaFull right
