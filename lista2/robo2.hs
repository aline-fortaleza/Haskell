data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)


facesAux :: Direction -> [Command] -> [Direction]
facesAux a [] = [a] 
facesAux North (TurnLeft:as) = West : facesAux West as
facesAux North (TurnRight:as) = East : facesAux East as
facesAux South (TurnLeft:as) = East : facesAux East as
facesAux South (TurnRight:as) = West : facesAux West as
facesAux West (TurnLeft:as) = South : facesAux South as
facesAux West (TurnRight:as) = North : facesAux North as
facesAux East (TurnLeft:as) = North : facesAux North as
facesAux East (TurnRight:as) = South : facesAux South as
facesAux a (_:as) = a : facesAux a as

faces :: Direction -> [Command] -> Direction
faces dir comm = last (facesAux dir comm)
