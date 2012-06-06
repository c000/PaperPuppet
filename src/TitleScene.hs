module TitleScene
  ( TitleScene (..)
  , titleScene
  ) where

import Data.Set
import Graphics.Rendering.OpenGL

import Class.GameScene as GS
import Internal.Texture
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
    textureBinding Texture2D $= Just lena
    preservingMatrix $ do
      translate (Vector3 (realToFrac x) (realToFrac y) 0 :: Vector3 GLfloat)
      renderPrimitive Quads $ do
        t2 0 1
        v2 0 0
        t2 1 1
        v2 100 0
        t2 1 0
        v2 100 100
        t2 0 0
        v2 0 100
    return ()

titleScene :: TitleScene
titleScene = TitleScene 0 (100,100)

v2 :: GLfloat -> GLfloat -> IO ()
v2 x y = vertex (Vertex2 x y)

t2 :: GLfloat -> GLfloat -> IO ()
t2 x y = texCoord (TexCoord2 x y)
