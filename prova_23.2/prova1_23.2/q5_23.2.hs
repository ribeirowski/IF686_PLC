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