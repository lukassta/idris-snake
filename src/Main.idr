module Main

import Data.Buffer
import Data.Fuel
import System

import Game
import IO

%default covering


main : IO ()
main = do
    setUp

    buff <- newBuffer 1

    usleep 3000000

    case buff of
        Nothing      => pure()
        Just keyBuff => gameLoop forever keyBuff newGameState

    cleanUp
