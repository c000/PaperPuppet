module GameStage.GameObject
  where

import Data.Complex

import Graphics.Rendering.OpenGL

import qualified Class.Sprite as S
import Internal.OpenGL
import Internal.Texture
import GlobalSettings

type Pos = Complex GLfloat

data GameObject = GameObject
  { pos :: Pos
  , radius :: Int
  , size :: Complex GLfloat
  , gameTexture :: Maybe TextureAnimation
  , offset :: Complex GLfloat
  , frame :: Int
  } deriving (Eq)

defaultGameObject
  = GameObject (fromIntegral windowWidth / 2
             :+ fromIntegral windowHeight / 2)
               0
               (0 :+ 0)
               Nothing
               (0 :+ 0)
               0

freeGameObject GameObject { gameTexture = ta }
  = case ta of
      Nothing -> return ()
      Just (TA t _ _) -> freeTexture [t]

class HaveGameObject a where
  gameObject :: a -> GameObject

renderFine bf c (GameObject { pos = x :+ y
                            , size = sx :+ sy
                            , gameTexture = tex
                            , offset = ox :+ oy
                            , frame = f
                            }) = do
  let hsx = sx / 2
      hsy = sy / 2
  case tex of
    Nothing -> do
      blendFunc $=! bf
      textureBinding Texture2D $=! Nothing
      preservingMatrix $ do
        translate $ Vector3 x y 0
        renderPrimitive Quads $ do
          color (c4 1 0 1 1)
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
      blendFunc $=! bf
      preservingMatrix $ do
        translate $ Vector3 (x+ox) (y+oy) 0
        renderPrimitive Quads $ do
          color c
          t2 tw0 (th0 + th)
          v2 (-hsx) (-hsy)
          t2 tw0 th0
          v2 (-hsx) (hsy)
          t2 (tw0 + tw) th0
          v2 (hsx) (hsy)
          t2 (tw0 + tw) (th0 + th)
          v2 (hsx) (-hsy)

instance S.Sprite GameObject where
  render = renderFine (SrcAlpha, OneMinusSrcAlpha) (c4 1 1 1 1)

  center (GameObject {pos = x :+ y})  = Position (round x) (round y)

  radius a = radius a
