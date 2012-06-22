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
  , gameTexture :: Maybe TextureAnimation
  , frame :: Int
  } deriving (Eq)

class HaveGameObject a where
  gameObject :: a -> GameObject

instance S.Sprite GameObject where
  render (GameObject { pos = x :+ y
                     , size = sx :+ sy
                     , gameTexture = tex
                     , frame = f
                     }) = do
    let hsx = sx / 2
        hsy = sy / 2
    case tex of
      Nothing -> do
        blendFunc $=! (One, Zero)
        preservingMatrix $ do
          translate $ Vector3 x y 0
          renderPrimitive Quads $ do
            c4 1 1 1 1
            v2 (hsx) (hsy)
            v2 (-hsx) (hsy)
            v2 (-hsx) (-hsy)
            v2 (hsx) (-hsy)
      Just (TA texture (divx, divy) fs) -> do
        let ImageTexture t w h = texture
            (fx, fy) = fs !! (f `mod` length fs)
            tw = w / fromIntegral divx
            th = h / fromIntegral divy
            tw0 = tw * fromIntegral fx
            th0 = th * fromIntegral fy
        textureBinding Texture2D $=! (Just t)
        blendFunc $=! (SrcAlpha, OneMinusSrcAlpha)
        preservingMatrix $ do
          translate $ Vector3 x y 0
          renderPrimitive Quads $ do
            c4 1 1 1 1
            t2 tw0 (th0 + th)
            v2 (hsx) (hsy)
            t2 tw0 th0
            v2 (-hsx) (hsy)
            t2 (tw0 + tw) th0
            v2 (-hsx) (-hsy)
            t2 (tw0 + tw) (th0 + th)
            v2 (hsx) (-hsy)


  center (GameObject {pos = x :+ y})  = Position (round x) (round y)

  radius a = radius a
