main = do
    s <- getLine
    let result = evalTree (read s)
    print result

data Ops = SUM | MUL | SUB
            deriving (Read)

data IntTree = Nilt Int | Node Ops IntTree IntTree
                deriving (Read)

evalTree :: IntTree -> Int
evalTree (Nilt x) = x
evalTree (Node SUM x1 x2) = evalTree x1 + evalTree x2
evalTree (Node MUL x1 x2) = evalTree x1 * evalTree x2
evalTree (Node SUB x1 x2) = evalTree x1 - evalTree x2