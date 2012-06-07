module GameStage
  ( GameStage
  , gameStage
  ) where

import Data.Set
import Graphics.Rendering.OpenGL

import Class.GameScene as GS
import Internal.Texture
import KeyBind
import GlobalValue

data GameStage = GameStage
  {
  } deriving Eq

instance GameScene GameStage where
  update (GV {keyset = key}) scene = do
    case member QUIT key of
      True  -> return EndScene
      False -> return $ GS.Replace scene

  render scene = do
    return ()

gameStage :: GameStage
gameStage = GameStage
