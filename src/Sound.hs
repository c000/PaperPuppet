module Sound
  ( MusicName (..)
  , Command (..)
  , SoundChan
  , getSoundThread
  , module Control.Concurrent.Chan
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan

import Graphics.UI.SDL.Mixer

data MusicData = MD
  { title :: Music
  }

data MusicName = Title
  deriving (Eq, Show)

data Command = Quit | StopMusic | Music MusicName
  deriving (Eq, Show)

type SoundChan = Chan Command

loadMD :: IO MusicData
loadMD = MD <$> loadMUS "res/title.ogg"

freeMD :: MusicData -> IO ()
freeMD (MD t) = do
  freeMusic t

getSoundThread :: IO SoundChan
getSoundThread = do
  chan <- newChan
  md <- loadMD
  _ <- forkIO $ soundThread chan md
  return chan

soundThread :: SoundChan -> MusicData -> IO ()
soundThread ch md = do
  command <- readChan ch
  case command of
    Quit -> return ()
    StopMusic -> do
      p <- playingMusic
      if p then fadeOutMusic 1000 else return ()
    Music musicName -> case musicName of
      Title -> playMusic (title md) (-1)
  if command /= Quit
    then soundThread ch md
    else do
      freeMD md
