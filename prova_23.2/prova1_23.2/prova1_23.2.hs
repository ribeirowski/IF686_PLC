--Questão 1

fibonacci :: [Int]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

fibonacciTeste :: Int -> [Int]
fibonacciTeste n = take n fibonacci

--Questão 2

merginho :: Ord t => t -> [t] -> [t]
merginho x [] = [x]
merginho x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : merginho x ys

merge :: Ord t => [t] -> [t] -> [t]
merge [] [] = []
merge [] (y:ys) = (y:ys)
merge (x:xs) [] = (x:xs)
merge (x:xs) (y:ys) = merginho x (merge xs (y:ys))

-- Questão 3

mergeSort :: Ord t => [t] -> [t]
mergeSort list = foldr merginho [] list

merginho :: Ord t => t -> [t] -> [t]
merginho x [] = [x]
merginho x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : merginho x ys

merge :: Ord t => [t] -> [t] -> [t]
merge [] [] = []
merge [] (y:ys) = (y:ys)
merge (x:xs) [] = (x:xs)
merge (x:xs) (y:ys) = merginho x (merge xs (y:ys))

-- Questão 4

type Pilha t = [t]

data Elemento = Valor Int | Soma | Multiplica
                deriving (Show)

geraString :: Pilha Elemento -> String
geraString pilha = case pilha of
    [] -> ""
    (Valor x) : (Valor y) : (Soma : xs) -> "(" ++ show x ++ " + " ++ show y ++ ")" ++ geraString xs
    (Valor x) : (Valor y) : (Multiplica : xs) -> "(" ++ show x ++ " * " ++ show y ++ ")" ++ geraString xs
    (Valor x) : xs -> geraString xs ++ show x
    (Soma : xs) -> "+" ++ geraString xs
    (Multiplica : xs) -> "*" ++ geraString xs

--OBS: funciona pro caso teste da questão :)

-- Questão 5

type Pilha t = [t]

data Elemento = Valor Int | Soma | Multiplica
                deriving (Show)

calculaExpressao :: Pilha Elemento -> Int
calculaExpressao pilha = case pilha of
    [] -> 0
    (Valor x) : [] -> x
    (Valor x) : (Valor y) : (Soma : xs) -> calculaExpressao (Valor (x + y) : xs)
    (Valor x) : (Valor y) : (Multiplica : xs) -> calculaExpressao (Valor (x * y) : xs)
    _ : xs -> calculaExpressao xs