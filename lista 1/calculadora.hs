-- Crie a função que recebe uma lista de tuplas com comandos e valores. A função deve começar pelo valor 0. Por exemplo, caso a sequência de comandos seja [("Multiplica", 2), ("Soma", 5), ("Subtrai", 3)] a função deve pegar 0 e efetuar as seguintes operações: (((0 * 2) + 5) - 3). Esses comandos podem ser "Multiplica", "Soma", "Subtrai" ou "Divide". Para o caso de uma divisão por 0, a função deve retornar o valor -666 independente de quanto tenha calculado até essa divisão.

type Comando = String
type Valor = Int

aux :: [(Comando, Valor)] -> Int -> Int
aux [] num = num
aux ((comando, valor):xs) num
    | comando == "Multiplica" = aux xs (num * valor)
    | comando == "Soma" = aux xs (num + valor)
    | comando == "Subtrai" = aux xs (num - valor)
    | comando == "Divide" && valor /= 0 = aux xs ( num `div` valor)
    | comando == "Divide" && valor == 0 = -666

executa :: [(Comando, Valor)] -> Int
executa [] = 0
executa ((comando, valor):xs) = aux ((comando, valor):xs) 0

main = do
    a <- getLine
    let result = executa (read a)
    print result