module Main

import System
import System.Concurrency
import System.File
import Data.IORef
import Data.Nat
import Data.List
import Data.List1

%default covering

Coordinates : Type
Coordinates = (Nat, Nat)


Snake : Type
Snake = (Nat, (List1 Coordinates))


data Direction =
    Up    |
    Right |
    Down  |
    Left


data GameState =
    Over |
    Active Direction Snake (List Coordinates)


CLEAR_SCREEN            = "\x1B[2J"
CLEAR_FROM_CURSOT       = "\x1B[K"
MOVE_CURSOR_TO_HOME     = "\x1B[H"
MOVE_CURSOR_TO_ZERO     = "\x1B[1;1H"
RESTORE_CURSOR_POSITION = "\x1B[u"
SAVE_CURSOR_POSITION    = "\x1B[s"
UPLINE                  = "\x1B[F"


snakeText : String
snakeText = """


     _____ _   _          _  ________ \r
    / ____| \\ | |   /\\   | |/ /  ____|\r
   | (___ |  \\| |  /  \\  | ' /| |__   \r
    \\___ \\| . ` | / /\\ \\ |  < |  __|  \r
    ____) | |\\  |/ ____ \\| . \\| |____ \r
   |_____/|_| \\_/_/    \\_\\_|\\_\\______|\r

"""


gameOverText : String
gameOverText = """


     _____          __  __ ______    ______      ________ _____    \r
    / ____|   /\\   |  \\/  |  ____|  / __ \\ \\    / /  ____|  __ \\   \r
   | |  __   /  \\  | \\  / | |__    | |  | \\ \\  / /| |__  | |__) |  \r
   | | |_ | / /\\ \\ | |\\/| |  __|   | |  | |\\ \\/ / |  __| |  _  /   \r
   | |__| |/ ____ \\| |  | | |____  | |__| | \\  /  | |____| | \\ \\   \r
    \\_____/_/    \\_\\_|  |_|______|  \\____/   \\/   |______|_|  \\_\\  \r

"""


setRaw : IO ()
setRaw = system "stty -echo raw" >>= \_ => pure ()


restore : IO ()
restore = system "stty echo cooked" >>= \_ => pure ()


getKey : IO Char
getKey = getChar


trim : List1 a -> Nat -> List1 a
trim (x ::: []) _     = (x ::: [])
trim (x ::: xs) 0     = (x ::: [])
trim (x ::: xs) (S k) = x ::: trim' xs k
where
    trim' : List a -> Nat -> List a
    trim' []        _     = []
    trim' (x :: xs) 0     = []
    trim' (x :: xs) (S k) = x :: trim' xs k


collides : Coordinates -> List Coordinates -> Bool
collides _ [] = False
collides coords1 (coords2 :: xs) =
    if coords1 == coords2
       then True
       else collides coords1 xs


inputThread : (quitRef: IORef Bool) -> (keyboardRef: IORef (Maybe Char)) -> IO ()
inputThread quitRef keyboardRef = do
    quit <- readIORef quitRef
    if quit
        then pure()
        else do
            c <- getChar
            writeIORef keyboardRef (Just c)
            inputThread quitRef keyboardRef


whatIn : Coordinates -> (snake: Snake) -> (fruits: List Coordinates) -> String
whatIn coords (len, spine) fruits =
    case elem coords spine of
        True  => "██"
        False => case elem coords fruits of
            True => "()"
            False => "░░"


drawScreen : (i: Nat) -> (j: Nat) -> (snake: Snake) -> (fruits: List Coordinates) -> IO ()
drawScreen 0 0 snake fruits = do
    putStr $ whatIn (0    , 0    ) snake fruits ++ "\n\r"
drawScreen 0 (S j) snake fruits = do
    putStr $ whatIn (0    , (S j)) snake fruits ++ "\n\r"
    drawScreen 10 j snake fruits
drawScreen (S i) j snake fruits = do
    putStr $ whatIn ((S i), j    ) snake fruits
    drawScreen i j snake fruits


renderGame : GameState -> IO ()
renderGame (Active _ snake fruits) = drawScreen 10 10 snake fruits
renderGame (Over)                  = pure ()


newCoordinates : Direction -> Coordinates -> Coordinates
newCoordinates Up    (x    , y    ) = (x               , (mod (y + 1) 11))
newCoordinates Right (0    , y    ) = (10              , y               )
newCoordinates Right ((S x), y    ) = ((mod x 11)      , y               )
newCoordinates Down  (x    , 0    ) = (x               , 10              )
newCoordinates Down  (x    , (S y)) = (x               , (mod y 11)      )
newCoordinates Left  (x    , y    ) = ((mod (x + 1) 11), y               )


eatFruit : (fruits: List Coordinates) -> Snake -> (Nat, (List Coordinates))
eatFruit fruits (len, (head ::: tail)) = case eatFruit' fruits head of
    (False, fruits) => (len    , fruits)
    (True,  fruits) => (len + 1, fruits)
where
    eatFruit' : (fruits: List Coordinates) -> (headCoords: Coordinates) -> (Bool, List Coordinates)
    eatFruit' [] _ = (False, [])
    eatFruit' (fruitCoords :: xs) headCoords =
        if fruitCoords == headCoords
            then (True, xs)
            else case eatFruit' xs headCoords of
                (bool, xs) => (bool, fruitCoords :: xs)


updateState : GameState -> GameState
updateState Over                                            = Over
updateState (Active direction (len, coords ::: xs) fruits) =
    let newCoords  = newCoordinates direction coords in
    let (newLen, newFruits) = eatFruit fruits (len, newCoords ::: coords :: xs) in
    case trim (newCoords ::: coords :: xs) newLen of
        (head ::: trimmedTail) =>
            if collides head trimmedTail
                then Over
                else Active direction (newLen, head ::: trimmedTail) newFruits


mainLoop : Fuel -> IORef (Maybe Char) -> (gameState: GameState) -> IO ()
mainLoop Dry _ _ = pure ()
mainLoop (More fuel) ref gameState = do
    let newGameState = updateState gameState

    putStr CLEAR_SCREEN
    putStr MOVE_CURSOR_TO_ZERO
    renderGame gameState

    usleep 400000

    key <- readIORef ref

    writeIORef ref Nothing
    fflush stdout

    case newGameState of
        Over => do
            putStr CLEAR_SCREEN
            putStr MOVE_CURSOR_TO_ZERO
            putStr gameOverText
            usleep 3000000
        Active direction snake fruits => case key of
            Just 'q' => putStrLn CLEAR_SCREEN
            Just 'w' => case direction of
                    Down  => mainLoop fuel ref $ Active direction snake fruits
                    _     => mainLoop fuel ref $ Active Up        snake fruits
            Just 'a' => case direction of
                    Right => mainLoop fuel ref $ Active direction snake fruits
                    _     => mainLoop fuel ref $ Active Left      snake fruits
            Just 's' => case direction of
                    Up    => mainLoop fuel ref $ Active direction snake fruits
                    _     => mainLoop fuel ref $ Active Down      snake fruits
            Just 'd' => case direction of
                    Left  => mainLoop fuel ref $ Active direction snake fruits
                    _     => mainLoop fuel ref $ Active Right     snake fruits
            _             => mainLoop fuel ref $ Active direction snake fruits


newSnake : Snake
newSnake = (3, ((5, 5) ::: []))


newFruits : List Coordinates
newFruits = [(1, 1), (6, 8), (5, 8)]


main : IO ()
main = do
    setRaw
    putStr MOVE_CURSOR_TO_ZERO
    putStr CLEAR_SCREEN
    putStr snakeText

    quitRef <- newIORef False
    keyboardRef <- newIORef Nothing

    _ <- fork $ inputThread quitRef keyboardRef

    usleep 3000000

    mainLoop forever keyboardRef $ Active Up newSnake newFruits
    writeIORef quitRef True

    putStr MOVE_CURSOR_TO_ZERO
    putStr CLEAR_SCREEN
    putStr "\rYou exited the game\rn"

    restore
    putStrLn ""
