module IO

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


public export
snakeText : String
snakeText = """


     _____ _   _          _  ________ \r
    / ____| \\ | |   /\\   | |/ /  ____|\r
   | (___ |  \\| |  /  \\  | ' /| |__   \r
    \\___ \\| . ` | / /\\ \\ |  < |  __|  \r
    ____) | |\\  |/ ____ \\| . \\| |____ \r
   |_____/|_| \\_/_/    \\_\\_|\\_\\______|\r

"""


public export
gameOverText : String
gameOverText = """


     _____          __  __ ______    ______      ________ _____    \r
    / ____|   /\\   |  \\/  |  ____|  / __ \\ \\    / /  ____|  __ \\   \r
   | |  __   /  \\  | \\  / | |__    | |  | \\ \\  / /| |__  | |__) |  \r
   | | |_ | / /\\ \\ | |\\/| |  __|   | |  | |\\ \\/ / |  __| |  _  /   \r
   | |__| |/ ____ \\| |  | | |____  | |__| | \\  /  | |____| | \\ \\   \r
    \\_____/_/    \\_\\_|  |_|______|  \\____/   \\/   |______|_|  \\_\\  \r

"""


public export
vicotryText : String
vicotryText = """


   __      _______ _____ _______ ____  _______     __  \r
   \\ \\    / /_   _/ ____|__   __/ __ \\|  __ \\ \\   / /  \r
    \\ \\  / /  | || |       | | | |  | | |__) \\ \\_/ /   \r
     \\ \\/ /   | || |       | | | |  | |  _  / \\   /    \r
      \\  /   _| || |____   | | | |__| | | \ \\  | |     \r
       \\/   |_____\\_____|  |_|  \\____/|_|  \\_\\ |_|     \r

"""



drainRead : Buffer -> Maybe Char -> IO (Maybe Char)
drainRead buff lastDrained = do
    n <- primIO $ prim_read 0 buff 1
    if n <= 0
        then pure lastDrained
        else do
            byte <- getBits8 buff 0
            drainRead buff (Just (chr (cast byte)))


public export
latestKey : Buffer -> IO (Maybe Char)
latestKey keyBuff = drainRead keyBuff Nothing


o_NONBLOCK : Int
o_NONBLOCK = 2048  -- Linux


public export
setUp : IO()
setUp = do
    ignore $ system "stty -echo raw"

    flags <- primIO $ prim_fcntl 0 3 0
    _     <- primIO $ prim_fcntl 0 4 (flags + o_NONBLOCK)

    putStr MOVE_CURSOR_TO_ZERO
    putStr CLEAR_SCREEN
    putStr snakeText


public export
cleanUp : IO()
cleanUp = do
    ignore $ system "stty echo cooked"

    putStr MOVE_CURSOR_TO_ZERO
    putStr CLEAR_SCREEN
