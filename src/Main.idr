module Main

import System
import System.Concurrency
import System.File
import Data.IORef
import Data.Nat
import Data.List
import Data.List1

%default covering

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
data Coordinates = Coord Nat Nat
data Snake = Snek Nat (List1 Coordinates)

data Direction =
    Up    |
    Right |
    Down  |
    Left

data GameState =
    Over |
    Active Direction Snake

setRaw : IO ()
setRaw = system "stty -echo raw" >>= \_ => pure ()

restore : IO ()
restore = system "stty echo cooked" >>= \_ => pure ()

getKey : IO Char
getKey = getChar

--popLast : List a -> List a
--popLast [] = []
--popLast (x :: []) = []
--popLast (x :: xs) = x :: popLast xs

trim : List1 a -> Nat -> List1 a
trim (x ::: []) _ = (x ::: [])
trim (x ::: xs) 0 = (x ::: [])
trim (x ::: xs) (S k) = x ::: trim' xs k
where
    trim' : List a -> Nat -> List a
    trim' [] _ = []
    trim' (x :: xs) 0 = []
    trim' (x :: xs) (S k) = x :: trim' xs k

collides : Coordinates -> List1 Coordinates -> Bool
collides (Coord x1 y1) ((Coord x2 y2) ::: xs) =
    if x1 == x2 && y1 == y2
       then True
       else collides' (Coord x1 y1) xs
where
    collides' : Coordinates -> List Coordinates -> Bool
    collides' _ [] = False
    collides' (Coord x1 y1) ((Coord x2 y2) :: xs) =
        if x1 == x2 && y1 == y2
           then True
           else collides' (Coord x1 y1) xs

inputThread : (quitRef: IORef Bool) -> (keyboardRef: IORef (Maybe Char)) -> IO ()
inputThread quitRef keyboardRef = do
    quit <- readIORef quitRef
    if quit
        then pure()
        else do
            c <- getChar
            writeIORef keyboardRef (Just c)
            inputThread quitRef keyboardRef

snakeIn : (snake: Snake) -> (x: Nat) -> (y: Nat) -> Bool
snakeIn (Snek len ((Coord i j) ::: xs)) x y =
    if i == x && j == y
        then True
        else snakeIn' xs x y
where
    snakeIn' : (List Coordinates) -> (x: Nat) -> (y: Nat) -> Bool
    snakeIn' [] _ _ = False
    snakeIn' ((Coord i j) :: xs) x y =
        if i == x && j == y
            then True
            else snakeIn' xs x y

drawScreen : (i: Nat) -> (j: Nat) -> (snake: Snake) -> IO ()
drawScreen 0 0 snake = do
    if snakeIn snake 0 0
        then putStr "██\n\r"
        else putStr "░░\n\r"
drawScreen 0 (S j) snake = do
    if snakeIn snake 0 (S j)
        then putStr "██\n\r"
        else putStr "░░\n\r"
    drawScreen 10 j snake
drawScreen (S i) j snake = do
    if snakeIn snake (S i) j
        then putStr "██"
        else putStr "░░"
    drawScreen i j snake

renderGame : GameState -> IO ()
renderGame (Active _ snake) = drawScreen 10 10 snake
renderGame (Over)           = pure ()

newCoordinates : Direction -> Coordinates -> Coordinates
newCoordinates Up    (Coord x     y    ) = Coord x                (mod (y + 1) 11)
newCoordinates Right (Coord 0     y    ) = Coord 10               y
newCoordinates Right (Coord (S x) y    ) = Coord (mod x 11)       y
newCoordinates Down  (Coord x     0    ) = Coord x                10
newCoordinates Down  (Coord x     (S y)) = Coord x                (mod y 11)
newCoordinates Left  (Coord x     y    ) = Coord (mod (x + 1) 11) y

eatFruit : (fruits: List Coordinates) -> Snake -> (Nat, (List Coordinates))
eatFruit [] (Snek len _)= (len, [])
eatFruit ((Coord x1 x2) :: xs) (Snek len (x ::: ys)) = ?eatFruit_rhs_5

updateState : GameState -> GameState
updateState Over                                         = Over
updateState (Active direction (Snek len (coords ::: xs))) =
    let newCoords  = newCoordinates direction coords in
    let (newLen, newFruit) = eatFruit [] (Snek len (newCoords ::: coords :: xs)) in
    case trim (newCoords ::: coords :: xs) len of
        [] => Over
        (head :: trimmedTail) =>
            if collides head trimmedTail
                then Over
                else Active direction $ Snek len $ head :: trimmedTail


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
        Active direction snake => case key of
            Just 'q' => putStrLn CLEAR_SCREEN
            Just 'w' => case direction of
                    Down  => mainLoop fuel ref $ Active direction snake
                    _     => mainLoop fuel ref $ Active Up        snake
            Just 'a' => case direction of
                    Right => mainLoop fuel ref $ Active direction snake
                    _     => mainLoop fuel ref $ Active Left      snake
            Just 's' => case direction of
                    Up    => mainLoop fuel ref $ Active direction snake
                    _     => mainLoop fuel ref $ Active Down      snake
            Just 'd' => case direction of
                    Left  => mainLoop fuel ref $ Active direction snake
                    _     => mainLoop fuel ref $ Active Right     snake
            _        => mainLoop fuel ref $ Active direction snake


newSnake : Snake
newSnake = Snek 9 ((Coord 5 5) ::: [])

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

    mainLoop forever keyboardRef $ Active Up newSnake
    writeIORef quitRef True

    putStr MOVE_CURSOR_TO_ZERO
    putStr CLEAR_SCREEN
    putStr "\rYou exited the game\rn"

    restore
    putStrLn ""

