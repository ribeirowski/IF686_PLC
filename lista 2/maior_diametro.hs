main = do
    s <- getLine
    let result = maiorDiametro (read s::Tree Int)
    print result

data Tree t = Nilt | Node t (Tree t) (Tree t)
            deriving (Read)

maiorDiametro :: Ord t => Tree t -> Int
maiorDiametro Nilt = 0
maiorDiametro (Node x left right) = max (max (maiorDiametro left) (maiorDiametro right)) (diametro (Node x left right))

altura :: Ord t => Tree t -> Int
altura Nilt = 0
altura (Node _ left right) = 1 + (max (altura left) (altura right))

diametro :: Ord t => Tree t -> Int
diametro Nilt = 0
diametro (Node _ left right) = 1 + (altura left) + (altura right)