module GameStage.GameObject
  where

import Data.Complex
import Data.Maybe

import Graphics.Rendering.OpenGL

import qualified Class.Sprite as S
import Internal.OpenGL
import Internal.Texture

type Pos = Complex GLfloat

data GameObject = GameObject
  { pos :: Pos
  , radius :: Int
  , size :: Complex GLfloat
  , gameTexture :: TextureAnimation
  , frame :: Int
  } deriving (Eq)

class HaveGameObject a where
  gameObject :: a -> GameObject

instance S.Sprite GameObject where
  render (GameObject { pos = x :+ y
                     , size = sx :+ sy
                     , gameTexture = TA tex (dx, dy) frames
                     , frame = frame
                     }) = do
    let hsx = sx / 2
        hsy = sy / 2
    blendFunc $=! (One, Zero)
    preservingMatrix $ do
      translate $ Vector3 x y 0
      renderPrimitive Quads $ do
        c4 1 1 1 1
        v2 (hsx) (hsy)
        v2 (-hsx) (hsy)
        v2 (-hsx) (-hsy)
        v2 (hsx) (-hsy)

  center (GameObject {pos = x :+ y})  = Position (round x) (round y)

  radius a = radius a
