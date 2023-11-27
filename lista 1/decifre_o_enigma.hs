decEnigma :: String -> [(Char, Char)] -> String
decEnigma [] str = []
decEnigma (x:xs) str = (translate str x) : decEnigma xs str

translate :: [(Char, Char)] -> Char -> Char
translate [] char = char
translate (x:xs) char
    | fst x == char = snd x
    | otherwise = translate xs char

main = do
    a <- getLine
    b <- getLine
    let result = decEnigma a (read b)
    print result