module Main

import Data.Buffer
import Data.Fuel
import System

import Game
import IO
import Types

%default covering


main : IO ()
main = do
    setUp

    buff <- newBuffer 1

    usleep 3000000

    newGameState <- generateNewGameState

    case buff of
        Nothing      => pure()
        Just keyBuff => gameLoop forever keyBuff newGameState

    cleanUp
