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
import Data.Array

import Graphics.UI.SDL.Mixer

data MusicData = MD
  { title :: Music
  , se :: Array Int Chunk
  }

data MusicName = Title
  deriving (Eq, Show)

data Command = Quit | StopMusic | Music MusicName | Select | Shoot
  deriving (Eq, Show)

type SoundChan = Chan Command

loadMD :: IO MusicData
loadMD = MD <$> loadMUS "res/title.ogg"
            <*> loadSE [ "res/select.wav"
                       , "res/fire.wav"
                       ]
  where
    loadSE :: [FilePath] -> IO (Array Int Chunk)
    loadSE f = fmap (listArray (0, length f - 1)) (mapM loadWAV f)

freeMD :: MusicData -> IO ()
freeMD (MD t _) = do
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
    Select -> playSE 0
    Shoot -> playSE 1
  if command /= Quit
    then soundThread ch md
    else do
      freeMD md
  where
    playSE seNum = do
      playChannel seNum ((se md) ! seNum) 0
      return ()
