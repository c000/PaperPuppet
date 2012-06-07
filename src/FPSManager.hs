module FPSManager
  ( waitGame
  ) where

import Data.IORef
import Data.Word
import System.IO.Unsafe

import Graphics.UI.SDL.Time

data FPSState = FS
  { lastTime :: Word32
  , wait :: Int
  }

state :: IORef FPSState
state = unsafePerformIO $ newIORef $ FS 0 (1000 `div` targetFPS)

targetFPS :: Int
targetFPS = 60

waitGame :: IO ()
waitGame = do
  c <- getTicks
  fs@(FS l w) <- readIORef state
  let timeDiff = fromIntegral $ c - l
      timeDiff :: Int
  case w - timeDiff of
    correctWait | correctWait < 0 -> return ()
                | otherwise       -> delay $ fromIntegral correctWait
  n <- getTicks
  writeIORef state $ fs { lastTime = n }
  let fps = 1000 / fromIntegral (n - l)
  return ()
