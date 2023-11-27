-- pega uma string com um numero binario e retorna o inteiro na base 10
btoi :: String -> Int
btoi [] = 0
btoi (x:xs) = (fromEnum x - fromEnum '0') * (2 ^ length xs) + btoi xs

main = do
    s <- getLine
    let result = btoi s
    print result