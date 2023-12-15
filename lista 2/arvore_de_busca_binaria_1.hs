main = do
    s <- getLine
    let result = isBST (read s::Tree Int)
    print result

data Tree t = Nilt | Node t (Tree t) (Tree t)
            deriving (Read)

isBST :: Ord t => Tree t -> Bool
isBST Nilt = True
isBST (Node x y z) = menorQue x y && maiorQue x z && isBST y && isBST z

menorQue :: Ord t => t -> Tree t -> Bool
menorQue _ Nilt = True
menorQue x (Node root left right) = (root < x) && menorQue x left && menorQue x right

maiorQue :: Ord t => t -> Tree t -> Bool
maiorQue _ Nilt = True
maiorQue x (Node root left right) = (root > x) && maiorQue x right && maiorQue x left