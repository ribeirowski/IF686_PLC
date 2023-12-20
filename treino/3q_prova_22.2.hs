data Letra = Unica Char | Repetida Char Int
            deriving Show

rlencodeLetrasCodigo :: String -> [Letra]
rlencodeLetrasCodigo [] = []
rlencodeLetrasCodigo (x:y:xs)
    | x == y = Repetida x ((ehTalLetra (x:xs) x) + 1) : rlencodeLetrasCodigo (dropWhile (==x) xs)
    | xs == [] = Unica x : Unica y : []
    | otherwise = Unica x : rlencodeLetrasCodigo (y:xs)

rldecodeLetrasCodigo :: [Letra] -> String
rldecodeLetrasCodigo [] = []
rldecodeLetrasCodigo (Unica x : Unica y : xs) = x : y : rldecodeLetrasCodigo xs
rldecodeLetrasCodigo (Unica x : xs) = x : rldecodeLetrasCodigo xs
rldecodeLetrasCodigo (Repetida x n : xs) = ehTalLetra' n x ++ rldecodeLetrasCodigo xs

ehTalLetra :: String -> Char -> Int
ehTalLetra [] _ = 0
ehTalLetra (x:xs) c = length (takeWhile (==c) (dropWhile (/=c) (x:xs)))

ehTalLetra' :: Int -> Char -> String
ehTalLetra' 0 _ = []
ehTalLetra' n c = c : ehTalLetra' (n-1) c