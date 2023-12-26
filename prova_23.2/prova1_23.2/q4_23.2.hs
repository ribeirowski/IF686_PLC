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