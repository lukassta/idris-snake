module Main

import System
import System.Concurrency
import System.File
import Data.IORef
import Data.Fin
import Data.Nat
import Data.List
import Data.List1

import System
import System.Clock
import Data.Buffer


import Helper
import Variables

%default covering

-- FIXME: Could not make non blocking getChar with purely Idris
%foreign "C:read,libc.so.6"    -- Linux
prim_read : Int -> Buffer -> Int -> PrimIO Int


%foreign "C:fcntl,libc.so.6"   -- Linux
prim_fcntl : Int -> Int -> Int -> PrimIO Int


Coordinate : Type
Coordinate = Fin screenSize


Coordinates : Type
Coordinates = (Coordinate, Coordinate)


Snake : Type
Snake = (Nat, (List1 Coordinates))


newSnake : Snake
newSnake =
    let (halfSize ** haldLRscrnSize) = divWProof screenSize 2 in
    (snakeLength, ((natToFinLT halfSize, natToFinLT halfSize) ::: []))


data Direction =
    Up
    | Right
    | Down
    | Left


data GameState =
    Active Direction Bool Snake (List Coordinates)
    | Over
    | Quit


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

o_NONBLOCK : Int
o_NONBLOCK = 2048  -- Linux


setNonBlocking : IO ()
setNonBlocking = do
    flags <- primIO $ prim_fcntl 0 3 0
    _     <- primIO $ prim_fcntl 0 4 (flags + o_NONBLOCK)
    pure ()


setRaw : IO ()
setRaw = system "stty -echo raw" >>= \_ => pure ()


restore : IO ()
restore = system "stty echo cooked" >>= \_ => pure ()


drainRead : Buffer -> Maybe Char -> IO (Maybe Char)
drainRead buff lastDrained = do
    n <- primIO $ prim_read 0 buff 1
    if n <= 0
        then pure lastDrained
        else do
            byte <- getBits8 buff 0
            drainRead buff (Just (chr (cast byte)))


latestKey : Buffer -> IO (Maybe Char)
latestKey keyBuff = drainRead keyBuff Nothing


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


whatIn : Coordinates -> (pattern: Bool) -> (snake: Snake) -> (fruits: List Coordinates) -> String
whatIn (x, y) pattern (len, spine) fruits =
    case elem (x, y) spine of
        False => case elem (x, y) fruits of
            True => "()"
            False => "░░"
        True  =>
            if pattern /= ((mod ((finToNat x) + (finToNat y)) 2) == 0)
               then "██"
               else "▓▓"


drawScreen : (i: Coordinate) -> (j: Coordinate) -> (pattern: Bool) -> (snake: Snake) -> (fruits: List Coordinates) -> String
drawScreen 0 0 pattern snake fruits =
    whatIn (0     , 0     ) pattern snake fruits
    ++ "\r\n"
drawScreen 0 (FS j) pattern snake fruits =
    whatIn (0     , (FS j)) pattern   snake fruits
    ++ "\r\n"
    ++ drawScreen last (weaken j) pattern snake fruits
drawScreen (FS i) j pattern snake fruits =
    whatIn ((FS i), j     ) pattern snake fruits
    ++ drawScreen (weaken i) j pattern snake fruits


renderGame : GameState -> String
renderGame (Active _  pattern snake fruits) = drawScreen last last pattern snake fruits
renderGame Over                             = gameOverText
renderGame Quit                             = ""


newCoordinates : Direction -> Coordinates -> Coordinates
newCoordinates Up    (x     , y     ) = (x       , finS y  )
newCoordinates Right (0     , y     ) = (last    , y       )
newCoordinates Right ((FS x), y     ) = (weaken x, y       )
newCoordinates Down  (x     , 0     ) = (x       , last    )
newCoordinates Down  (x     , (FS y)) = (x       , weaken y)
newCoordinates Left  (x     , y     ) = (finS x  , y       )


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
updateState Over                                                   = Over
updateState Quit                                                   = Quit
updateState (Active direction pattern (len, coords ::: xs) fruits) =
    let newCoords  = newCoordinates direction coords in
    let (newLen, newFruits) = eatFruit fruits (len, newCoords ::: coords :: xs) in
    case trim (newCoords ::: coords :: xs) newLen of
        (head ::: trimmedTail) =>
            if collides head trimmedTail
                then Over
                else Active direction (not pattern) (newLen, head ::: trimmedTail) newFruits


mainLoop : Fuel -> (keyBuff: Buffer) -> (gameState: GameState) -> IO ()
mainLoop Dry  _ _ = pure ()
mainLoop (More fuel) keyBuff gameState = do
    key <- latestKey keyBuff

    let manipulatedState =
        case gameState of
            Over => Over
            Quit => Quit
            Active direction pattern snake fruits => case key of
                Just 'q' => Quit
                Just 'w' => case direction of
                        Down  => Active direction pattern snake fruits
                        _     => Active Up        pattern snake fruits
                Just 'a' => case direction of
                        Right => Active direction pattern snake fruits
                        _     => Active Left      pattern snake fruits
                Just 's' => case direction of
                        Up    => Active direction pattern snake fruits
                        _     => Active Down      pattern snake fruits
                Just 'd' => case direction of
                        Left  => Active direction pattern snake fruits
                        _     => Active Right     pattern snake fruits
                _             => Active direction pattern snake fruits

    let updatedState = updateState manipulatedState

    putStr $ CLEAR_SCREEN ++ MOVE_CURSOR_TO_ZERO ++ renderGame updatedState

    case updatedState of
        Over        => usleep 3000000
        Quit        => pure ()
        activeState => do
            usleep $ div 1000000 fps
            mainLoop fuel keyBuff activeState


newFruits : List Coordinates
newFruits = [(1, 1), (6, 8), (5, 8)]


main : IO ()
main = do
    setRaw
    setNonBlocking

    putStr MOVE_CURSOR_TO_ZERO
    putStr CLEAR_SCREEN
    putStr snakeText

    buff <- newBuffer 1

    usleep 3000000

    case buff of
        Nothing      => pure()
        Just keyBuff => mainLoop forever keyBuff $ Active Up False newSnake newFruits

    putStr MOVE_CURSOR_TO_ZERO
    putStr CLEAR_SCREEN

    restore
