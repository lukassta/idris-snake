module Game

import Data.Buffer
import Data.Fin
import Data.Fuel
import Data.List
import Data.List1
import System
import System.Random

import Helper
import IO
import Types
import Variables

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


newCoordinates : Direction -> Coordinates -> Coordinates
newCoordinates Up    (x     , y     ) = (x       , finS y  )
newCoordinates Right (0     , y     ) = (last    , y       )
newCoordinates Right ((FS x), y     ) = (weaken x, y       )
newCoordinates Down  (x     , 0     ) = (x       , last    )
newCoordinates Down  (x     , (FS y)) = (x       , weaken y)
newCoordinates Left  (x     , y     ) = (finS x  , y       )

growFruits : (fruits: List Coordinates) -> (spine: List Coordinates) -> IO (List Coordinates)
growFruits fruits spine = do
    let fullCoordinates = populate (last, last)
    let withoutFruits   = removeMatcingElements fullCoordinates fruits
    let withoutAll      = removeMatcingElements withoutFruits   spine
    maybeNewFruit <- randomElem withoutAll
    case maybeNewFruit of
        Nothing       => pure $ fruits
        Just newFruit => pure $ newFruit :: fruits



eatFruit : (fruits: List Coordinates) -> Snake -> IO (Nat, (List Coordinates))
eatFruit fruits (len, (head ::: tail)) = case eatFruit' fruits head of
    (False, fruits) => pure (len    , fruits)
    (True,  fruits) => do
        newFruit <- growFruits fruits (head :: tail)
        pure (len + 1, newFruit)
where
    eatFruit' : (fruits: List Coordinates) -> (headCoords: Coordinates) -> (Bool, List Coordinates)
    eatFruit' [] _ = (False, [])
    eatFruit' (fruitCoords :: xs) headCoords =
        if fruitCoords == headCoords
            then (True, xs)
            else case eatFruit' xs headCoords of
                (bool, xs) => (bool, fruitCoords :: xs)


getTileCount : Coordinates -> Nat
getTileCount (x, y) = mult ((finToNat x) + 1) ((finToNat y) + 1)


updateState : GameState -> IO GameState
updateState Over                                                   = pure Over
updateState Quit                                                   = pure Quit
updateState Victory                                                = pure Victory
updateState (Active direction pattern (len, coords ::: xs) fruits) = do
    let tileCount = getTileCount (last, last)
    let newCoords  = newCoordinates direction coords
    (newLen, newFruits) <- eatFruit fruits (len, newCoords ::: coords :: xs)
    if tileCount  <= newLen
        then pure Victory
        else
            case trim (newCoords ::: coords :: xs) newLen of
                (head ::: trimmedTail) =>
                    if collides head trimmedTail
                        then pure Over
                        else pure $ Active direction (not pattern) (newLen, head ::: trimmedTail) newFruits


renderCoordinate : Coordinates -> (pattern: Bool) -> (snake: Snake) -> (fruits: List Coordinates) -> String
renderCoordinate (x, y) pattern (len, spine) fruits =
    case elem (x, y) spine of
        False => case elem (x, y) fruits of
            True => "()"
            False => "░░"
        True  =>
            if pattern /= ((mod ((finToNat x) + (finToNat y)) 2) == 0)
               then "██"
               else "▓▓"


renderActive : (i: Coordinate) -> (j: Coordinate) -> (pattern: Bool) -> (snake: Snake) -> (fruits: List Coordinates) -> String
renderActive 0 0 pattern snake fruits =
    renderCoordinate (0     , 0     ) pattern snake fruits
    ++ "\r\n"
renderActive 0 (FS j) pattern snake fruits =
    renderCoordinate (0     , (FS j)) pattern   snake fruits
    ++ "\r\n"
    ++ renderActive last (weaken j) pattern snake fruits
renderActive (FS i) j pattern snake fruits =
    renderCoordinate ((FS i), j     ) pattern snake fruits
    ++ renderActive (weaken i) j pattern snake fruits


renderGame : GameState -> String
renderGame (Active _  pattern snake fruits) = renderActive last last pattern snake fruits
renderGame Over                             = gameOverText
renderGame Quit                             = ""
renderGame Victory                          = vicotryText


public export
gameLoop : Fuel -> (keyBuff: Buffer) -> (gameState: GameState) -> IO ()
gameLoop Dry  _ _ = pure ()
gameLoop (More fuel) keyBuff gameState = do
    key <- latestKey keyBuff

    let manipulatedState =
        case gameState of
            Over                                  => Over
            Quit                                  => Quit
            Victory                               => Victory
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

    updatedState <- updateState manipulatedState

    putStr $ CLEAR_SCREEN ++ MOVE_CURSOR_TO_ZERO ++ renderGame updatedState

    case updatedState of
        Over        => usleep 3000000
        Quit        => pure ()
        Victory     => usleep 3000000
        activeState => do
            usleep $ div 1000000 fps
            gameLoop fuel keyBuff activeState


generateNewFruits : Nat -> (fruits: List Coordinates) -> (snake: Snake) -> IO (List Coordinates)
generateNewFruits 0     fruits (len, (head ::: spine)) = do
    let fullCoordinates = populate (last, last)
    let withoutFruits   = removeMatcingElements fullCoordinates fruits
    let withoutAll      = removeMatcingElements fullCoordinates (head :: spine)
    maybeNewFruit <- randomElem withoutAll
    case maybeNewFruit  of
         Just newFruit => pure $ newFruit :: fruits
         Nothing       => pure $ fruits
generateNewFruits (S k) fruits (len, (head ::: spine)) = do
    let fullCoordinates = populate (last, last)
    let withoutFruits   = removeMatcingElements fullCoordinates fruits
    let withoutAll      = removeMatcingElements fullCoordinates (head :: spine)
    maybeNewFruit <- randomElem withoutAll
    case maybeNewFruit  of
         Just newFruit => do
            newFruits <- generateNewFruits  k (newFruit :: fruits) (len, (head ::: spine))
            pure $ newFruits
         Nothing       => pure $ fruits


newSnake : Snake
newSnake =
    let (halfSize ** haldLRscrnSize) = divWProof screenSize 2 in
    (snakeLength, ((natToFinLT halfSize, natToFinLT halfSize) ::: []))


public export
generateNewGameState : IO GameState
generateNewGameState = do
    newFruits <- (generateNewFruits fruitCount [] newSnake)
    pure $ Active Up False newSnake newFruits
