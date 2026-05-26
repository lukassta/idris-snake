import Data.Fin
import Data.List
import Data.List1

import Variables


Coordinate : Type
Coordinate = Fin screenSize


Coordinates : Type
Coordinates = (Coordinate, Coordinate)


Snake : Type
Snake = (Nat, (List1 Coordinates))




data Direction =
    Up
    | Right
    | Down
    | Left


data GameState =
    Active Direction Bool Snake (List Coordinates)
    | Over
    | Quit
