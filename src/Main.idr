module Main

import Data.Buffer
import Data.Fin
import Data.Fuel
import Data.List
import Data.List1
import Data.Nat
import System

import Game
import Helper
import IO
import Types
import Variables

%default covering

newFruits : List Coordinates
newFruits = [(1, 1), (6, 8), (5, 8)]


newSnake : Snake
newSnake =
    let (halfSize ** haldLRscrnSize) = divWProof screenSize 2 in
    (snakeLength, ((natToFinLT halfSize, natToFinLT halfSize) ::: []))


main : IO ()
main = do
    setUp

    buff <- newBuffer 1

    usleep 3000000

    case buff of
        Nothing      => pure()
        Just keyBuff => mainLoop forever keyBuff $ Active Up False newSnake newFruits

    cleanUp
