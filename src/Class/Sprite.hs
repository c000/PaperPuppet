module Class.Sprite
  where

import Graphics.Rendering.OpenGL

class Sprite a where
  render :: a -> IO ()

  center :: a -> Position

  radius :: a -> Int
  radius _ = 0

within :: (Sprite a, Sprite b) => a -> b -> Bool
within a b = (x1 - x2)^2 + (y1 - y2) ^2 < distance
  where
    Position x1 y1 = center a
    Position x2 y2 = center b
    distance       = fromIntegral $ (radius a + radius b)^2
