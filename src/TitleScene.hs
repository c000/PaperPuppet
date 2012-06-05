module TitleScene
  ( TitleScene (..)
  , titleScene
  ) where

import Data.Set
import Graphics.Rendering.OpenGL

import Class.GameScene as GS
import KeyBind
import GlobalValue

data TitleScene = TitleScene
  { frames :: Int
  , pos :: (Int, Int)
  } deriving Eq

instance GameScene TitleScene where
  update (GV {keyset = key}) (TitleScene frame (x,y)) = do
    if member QUIT key
      then return EndScene
      else return $ GS.Replace (TitleScene (frame+1) (x + 10*dx, y + 10*dy))
    where
      (dx,dy) = keysetToXY key

  render (TitleScene frame (x,y)) = do
    preservingMatrix $ do
      translate (Vector3 (realToFrac x) (realToFrac y) 0 :: Vector3 GLfloat)
      renderPrimitive Quads $ do
        v2 0 0
        v2 10 0
        v2 10 10
        v2 0 10
    return ()

titleScene :: TitleScene
titleScene = TitleScene 0 (100,100)

v2 :: GLfloat -> GLfloat -> IO ()
v2 x y = vertex (Vertex2 x y)
