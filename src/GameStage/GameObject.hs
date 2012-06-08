module GameStage.GameObject
  where

import Data.Complex

import Graphics.Rendering.OpenGL

import qualified Class.Sprite as S
import Internal.OpenGL

type Pos = Complex GLfloat

data GameObject = GameObject
  { pos :: Pos
  , radius :: Int
  , size :: Complex GLfloat
  } deriving Eq

instance S.Sprite GameObject where
  render (GameObject { pos = x :+ y
                     , size = sx :+ sy
                     }) = do
    let hsx = sx / 2
        hsy = sy / 2
    preservingMatrix $ do
      translate $ Vector3 x y 0
      renderPrimitive Quads $ do
        c3 1 1 1
        v2 (hsx) (hsy)
        v2 (-hsx) (hsy)
        v2 (-hsx) (-hsy)
        v2 (hsx) (-hsy)
  center (GameObject {pos = x :+ y})  = Position (round x) (round y)
  radius a = radius a
