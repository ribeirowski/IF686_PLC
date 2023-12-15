{- main = do
    s <- getLine
    let result = maiorDiametro (read s::Tree Int)
    print result -}

data Tree t = Nilt | Node t (Tree t) (Tree t)
            deriving (Read)

{- maiorDiametro :: Ord t => Tree t -> Int -}

noLeft :: Ord t => Tree t -> Int
noLeft Nilt = 0
noLeft (Node root Nilt Nilt) = 0
noLeft (Node root Nilt right) = 1 + noLeft right
noLeft (Node root left Nilt) = 1 + noLeft left
noLeft (Node root left right) = 1 + noLeft left + noRight left

noRight :: Ord t => Tree t -> Int
noRight Nilt = 0
noRight (Node root Nilt Nilt) = 0
noRight (Node root Nilt right) = 1 + noRight right
noRight (Node root left Nilt) = 1 + noRight left
noRight (Node root left right) = 1 + noRight right + noLeft right