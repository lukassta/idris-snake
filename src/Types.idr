module Types

import Data.Fin
import Data.List
import Data.List1

import Variables


public export
Coordinate : Type
Coordinate = Fin screenSize


public export
Coordinates : Type
Coordinates = (Coordinate, Coordinate)


public export
Snake : Type
Snake = (Nat, (List1 Coordinates))


public export
data Direction =
    Up
    | Right
    | Down
    | Left


public export
data GameState =
    Active Direction Bool Snake (List Coordinates)
    | Victory
    | Over
    | Quit
