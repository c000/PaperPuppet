module TitleScene where

import Data.Set
import Graphics.Rendering.OpenGL

import Class.GameScene as GS
import KeyBind
import GlobalValue

titleScene :: TitleScene
titleScene = TitleScene 0

data TitleScene = TitleScene
  { frames :: Int
  } deriving Eq

instance GameScene TitleScene where
  update (GV {keyset = key}) (TitleScene frame) = do
    if member QUIT key
      then return EndScene
      else return $ GS.Replace (TitleScene $ frame+1)

  render (TitleScene frame) = do
    renderPrimitive Lines $ do
      v2 0 0
      v2 (100 * cos fr) (100 * sin fr)
    return ()
      where
        fr = (realToFrac frame) / 100

v2 :: GLfloat -> GLfloat -> IO ()
v2 x y = vertex (Vertex2 x y)
