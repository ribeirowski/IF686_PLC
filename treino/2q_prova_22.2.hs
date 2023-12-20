rlencodeLetras :: String -> String
rlencodeLetras [] = []
rlencodeLetras (x:y:xs)
    | x == y = x : toEnum((ehTalLetra (x:xs) x) + 49) : rlencodeLetras (dropWhile (==x) xs)
    | xs == [] = x : y : []
    | otherwise = x : rlencodeLetras (y:xs)

rldecodeLetras :: String -> String
rldecodeLetras [] = []
rldecodeLetras (x:y:xs)
    | y == '0' || y == '1' || y == '2' || y == '3' || y == '4' || y == '5' || y == '6' || y == '7' || y == '8' || y == '9' = ehTalLetra' (fromEnum y - 48) x ++ rldecodeLetras xs
    | xs == [] = x : y : []
    | otherwise = x : rldecodeLetras (y:xs)

ehTalLetra :: String -> Char -> Int
ehTalLetra [] _ = 0
ehTalLetra (x:xs) c = length (takeWhile (==c) (dropWhile (/=c) (x:xs)))

ehTalLetra' :: Int -> Char -> String
ehTalLetra' 0 _ = []
ehTalLetra' n c = c : ehTalLetra' (n-1) c