module TitleScene
  ( TitleScene (..)
  , titleScene
  ) where

import Control.Applicative
import Control.Monad
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

data Objects = Objs
  { background :: GO.GameObject
  , rim :: GO.GameObject
  , cursor :: GO.GameObject
  , startButton :: GO.GameObject
  , endButton :: GO.GameObject
  } deriving Eq


data TitleScene = TitleScene
  { state :: TitleState
  , movable :: Bool
  , time :: Int
  , objects :: Objects
  } deriving Eq

instance GameScene TitleScene where
  dispose TitleScene { objects = Objs bg r c sb eb } = do
    GO.freeGameObject bg
    GO.freeGameObject r
    GO.freeGameObject c
    GO.freeGameObject sb
    GO.freeGameObject eb

  start (GV {sound = s}) _ = do
    SO.writeChan s (SO.Music SO.Title)

  update gv@GV{keyset = key} title = do
    case member QUIT key of
      True  -> return EndScene
      False -> (pressStart gv <=< moveState gv) $
               ( moveCursor
               . (\title@TitleScene {time = t} -> title {time = t+1})
               ) title
    where
      moveCursor t@TitleScene { state = st
                              , objects = o@Objs { cursor = c
                                                 }
                              }
        = let target = case st of
                         GameStart -> 120 :+ 200
                         GameEnd   -> 420 :+ 200
              current = GO.pos c
          in t { objects = o { cursor = c { GO.pos = current + (target-current)*0.2 }}}

  render TitleScene { state = st
                    , objects = Objs
                        { background = bg
                        , rim = r
                        , cursor = c
                        , startButton = sb
                        , endButton = eb
                        }
                    }
    = do
      SP.render bg
      case st of
        GameStart -> do
          renderBold $ sb { GO.pos = 250 :+ 200 }
          SP.render $ eb { GO.pos = 550 :+ 200 }
          SP.render $ c
        GameEnd   -> do
          SP.render $ sb { GO.pos = 250 :+ 200 }
          renderBold $ eb { GO.pos = 550 :+ 200 }
          SP.render $ c
      SP.render r
      where
        renderBold = GO.renderFine (SrcAlpha, OneMinusSrcAlpha) (c4 1 0.8 0.8 1)

titleScene :: IO TitleScene
titleScene = do
  o <- Objs <$> loadBG <*> loadRim <*> loadCursor
            <*> loadImageSize 200 100 "res/start.png"
            <*> loadImageSize 200 100 "res/end.png"
  return $ TitleScene GameStart True 0 o
  where
    loadWindowSize n = do
      t <- loadTexture n
      return $ GO.defaultGameObject { GO.size = 800 :+ 600
                                    , GO.gameTexture = Just $ TA t (1,1) [(0,0)]
                                    }
    loadImageSize w h n = do
      t <- loadTexture n
      return $ GO.defaultGameObject { GO.size = w :+ h
                                    , GO.gameTexture = Just $ TA t (1,1) [(0,0)]
                                    }
    loadRim = loadWindowSize "res/rim.png"
    loadBG = loadWindowSize "res/title.png"
    loadCursor = do
      t <- loadTexture "res/player.png"
      return $ GO.defaultGameObject { GO.pos = 0 :+ 0
                                    , GO.size = 70 :+ 600
                                    , GO.gameTexture = Just $ TA t (1,1) [(0,0)]
                                    , GO.offset = 0 :+ (-255)
                                    }

moveState :: GlobalValue -> TitleScene -> IO TitleScene
moveState GV { keyset = key
             , sound = s
             }
          title@TitleScene { state = st
                           , movable = m
                           }
  | member RB key = do playSelect 
                       return $ title { state = nst , movable = False }
  | member LB key = do playSelect
                       return $ title { state = pst , movable = False }
  | otherwise     = return $ title { state = st  , movable = True  }
  where
    nst = if m then next st else st
    pst = if m then prev st else st
    playSelect = SO.writeChan s SO.Select

pressStart :: GlobalValue -> TitleScene -> IO Result
pressStart GV { keyset = key
              , sound = s
              }
           title@TitleScene { state = st }
  | member A key = case st of
      GameStart -> do
        SO.writeChan s SO.StopMusic
        AddScene <$> gameStage
      GameEnd   -> return EndScene
  | otherwise = return $ GS.Replace title
