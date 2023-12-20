main = do
    a <- getLine
    b <- getLine
    let result = destination (read a) (read b)
    print result

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
                deriving (Eq, Show, Read)

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination (a,b) xs = faceNorth (a,b) xs

faceNorth :: (Int,Int) -> [Command] -> (Int,Int)
faceNorth (a,b) [] = (a,b)
faceNorth (a,b) (Forward x:xs) = faceNorth (a,b+x) xs
faceNorth (a,b) (Backward x:xs) = faceNorth (a,b-x) xs
faceNorth (a,b) (TurnLeft:xs) = faceLeft1 (a,b) xs
faceNorth (a,b) (TurnRight:xs) = faceRight1 (a,b) xs

faceSouth :: (Int,Int) -> [Command] -> (Int,Int)
faceSouth (a,b) [] = (a,b)
faceSouth (a,b) (Forward x:xs) = faceSouth (a,b-x) xs
faceSouth (a,b) (Backward x:xs) = faceSouth (a,b+x) xs
faceSouth (a,b) (TurnLeft:xs) = faceLeft3 (a,b) xs
faceSouth (a,b) (TurnRight:xs) = faceRight1 (a,b) xs

faceLeft1 :: (Int,Int) -> [Command] -> (Int,Int)
faceLeft1 (a,b) [] = (a,b)
faceLeft1 (a,b) (Forward x:xs) = faceLeft1 (a-x,b) xs
faceLeft1 (a,b) (Backward x:xs) = faceLeft1 (a+x,b) xs
faceLeft1 (a,b) (TurnLeft:xs) = faceLeft2 (a,b) xs
faceLeft1 (a,b) (TurnRight:xs) = faceNorth (a,b) xs

faceLeft2 :: (Int,Int) -> [Command] -> (Int,Int)
faceLeft2 (a,b) [] = (a,b)
faceLeft2 (a,b) (Forward x:xs) = faceLeft2 (a,b-x) xs
faceLeft2 (a,b) (Backward x:xs) = faceLeft2 (a,b+x) xs
faceLeft2 (a,b) (TurnLeft:xs) = faceLeft3 (a,b) xs
faceLeft2 (a,b) (TurnRight:xs) = faceLeft1 (a,b) xs

faceLeft3 :: (Int,Int) -> [Command] -> (Int,Int)
faceLeft3 (a,b) [] = (a,b)
faceLeft3 (a,b) (Forward x:xs) = faceLeft3 (a+x,b) xs
faceLeft3 (a,b) (Backward x:xs) = faceLeft3 (a-x,b) xs
faceLeft3 (a,b) (TurnLeft:xs) = faceNorth (a,b) xs
faceLeft3 (a,b) (TurnRight:xs) = faceLeft2 (a,b) xs

faceRight1 :: (Int,Int) -> [Command] -> (Int,Int)
faceRight1 (a,b) [] = (a,b)
faceRight1 (a,b) (Forward x:xs) = faceRight1 (a+x,b) xs
faceRight1 (a,b) (Backward x:xs) = faceRight1 (a-x,b) xs
faceRight1 (a,b) (TurnLeft:xs) = faceNorth (a,b) xs
faceRight1 (a,b) (TurnRight:xs) = faceRight2 (a,b) xs

faceRight2 :: (Int,Int) -> [Command] -> (Int,Int)
faceRight2 (a,b) [] = (a,b)
faceRight2 (a,b) (Forward x:xs) = faceRight2 (a,b-x) xs
faceRight2 (a,b) (Backward x:xs) = faceRight2 (a,b+x) xs
faceRight2 (a,b) (TurnLeft:xs) = faceRight1 (a,b) xs
faceRight2 (a,b) (TurnRight:xs) = faceRight3 (a,b) xs

faceRight3 :: (Int,Int) -> [Command] -> (Int,Int)
faceRight3 (a,b) [] = (a,b)
faceRight3 (a,b) (Forward x:xs) = faceRight3 (a-x,b) xs
faceRight3 (a,b) (Backward x:xs) = faceRight3 (a+x,b) xs
faceRight3 (a,b) (TurnLeft:xs) = faceRight2 (a,b) xs
faceRight3 (a,b) (TurnRight:xs) = faceNorth (a,b) xs