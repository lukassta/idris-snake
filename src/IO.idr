import Data.Buffer
import System

-- FIXME: Could not make non blocking getChar with purely Idris
%foreign "C:read,libc.so.6"    -- Linux
prim_read : Int -> Buffer -> Int -> PrimIO Int


%foreign "C:fcntl,libc.so.6"   -- Linux
prim_fcntl : Int -> Int -> Int -> PrimIO Int


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


setUp : IO()
setUp = do
    setRaw
    setNonBlocking

    putStr MOVE_CURSOR_TO_ZERO
    putStr CLEAR_SCREEN
    putStr snakeText


cleanUp : IO()
cleanUp = do
    putStr MOVE_CURSOR_TO_ZERO
    putStr CLEAR_SCREEN

    restore
