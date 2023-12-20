main = do
    a <- getLine
    b <- getLine
    let result = faces (read a) (read b)
    print result

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
                deriving (Eq, Show, Read)

data Direction = North | South | West | East
                deriving (Read, Show)

faces :: Direction -> [Command] -> Direction
faces North xs = faceNorth North xs
faces South xs = faceSouth South xs
faces West xs = faceWest West xs
faces East xs = faceEast East xs

faceNorth :: Direction -> [Command] -> Direction
faceNorth North [] = North
faceNorth North (TurnLeft:xs) = faceWest West xs
faceNorth North (TurnRight:xs) = faceEast East xs
faceNorth North (Forward x:xs) = faceNorth North xs
faceNorth North (Backward x:xs) = faceNorth North xs

faceSouth :: Direction -> [Command] -> Direction
faceSouth South [] = South
faceSouth South (TurnLeft:xs) = faceEast East xs
faceSouth South (TurnRight:xs) = faceWest West xs
faceSouth South (Forward x:xs) = faceSouth South xs
faceSouth South (Backward x:xs) = faceSouth South xs

faceWest :: Direction -> [Command] -> Direction
faceWest West [] = West
faceWest West (TurnLeft:xs) = faceSouth South xs
faceWest West (TurnRight:xs) = faceNorth North xs
faceWest West (Forward x:xs) = faceWest West xs
faceWest West (Backward x:xs) = faceWest West xs

faceEast :: Direction -> [Command] -> Direction
faceEast East [] = East
faceEast East (TurnLeft:xs) = faceNorth North xs
faceEast East (TurnRight:xs) = faceSouth South xs
faceEast East (Forward x:xs) = faceEast East xs
faceEast East (Backward x:xs) = faceEast East xs
