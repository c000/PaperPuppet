module TitleScene
  ( TitleScene (..)
  , titleScene
  ) where

import Control.Applicative
import Data.Set
import Data.Complex
import Graphics.Rendering.OpenGL

import Class.GameScene as GS
import Class.Sprite as SP
import Internal.Texture
import Internal.OpenGL
import KeyBind
import GlobalValue
import GameStage
import qualified GameStage.GameObject as GO
import Sound as SO

data TitleState = GameStart | GameEnd
  deriving (Eq, Enum, Bounded, Show)

next :: TitleState -> TitleState
next x
  | x == maxBound = minBound
  | otherwise     = succ x

prev :: TitleState -> TitleState
prev x
  | x == minBound = maxBound
  | otherwise     = pred x

data TitleScene = TitleScene
  { state :: TitleState
  , movable :: Bool
  , time :: Int
  , cursor :: GO.GameObject
  } deriving Eq

instance GameScene TitleScene where
  dispose TitleScene { cursor = c } = do
    GO.freeGameObject c

  start (GV {sound = s}) _ = do
    SO.writeChan s (SO.Music SO.Title)

  update (GV {keyset = key, sound = s}) title = do
    case member QUIT key of
      True  -> return EndScene
      False -> ( pressStart key s
               . moveCursor key
               . (\title@TitleScene {time = t} -> title {time = t+1})
               ) title

  render TitleScene { state = st
                    , cursor = c
                    }
    = do
      preservingMatrix $ do
        let ImageTexture tex w h = unsafeLoadTexture "res/title.png"
        textureBinding Texture2D $=! (Just $ tex)
        blendFunc $=! (One, Zero)
        renderPrimitive Quads $ do
          c4 0.5 0.5 0.5 1
          t2 0 h
          v2 0 0
          t2 w h
          v2 800 0
          t2 w 0
          v2 800 600
          t2 0 0
          v2 0 600
      case st of
        GameStart -> SP.render $ c { GO.pos = 100 :+ 200 }
        GameEnd   -> SP.render $ c { GO.pos = 100 :+ 100 }

titleScene :: IO TitleScene
titleScene = do
  c <- loadCursor
  return $ TitleScene GameStart True 0 c
  where
    loadCursor = do
      t <- loadTexture "res/cursor.png"
      return $ GO.defaultGameObject { GO.size = 100 :+ 100
                                    , GO.gameTexture = Just $ TA t (1,1) [(0,0)]
                                    }

moveCursor :: Keyset -> TitleScene -> TitleScene
moveCursor key title@(TitleScene { state = st, movable = m })
  | member UB key = title { state = nst , movable = False }
  | member DB key = title { state = pst , movable = False }
  | otherwise     = title { state = st  , movable = True  }
  where
    nst = if m then next st else st
    pst = if m then prev st else st

pressStart :: Keyset -> SO.SoundChan -> TitleScene -> IO Result
pressStart key s title@(TitleScene { state = st })
  | member A key = case st of
      GameStart -> do
        SO.writeChan s SO.StopMusic
        AddScene <$> gameStage
      GameEnd   -> return EndScene
  | otherwise = return $ GS.Replace title

