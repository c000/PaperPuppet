module GameStage.Player
  ( Player (..)
  , player
  , update
  ) where

import Graphics.Rendering.OpenGL (GLfloat)
import Data.Complex
import GameStage.GameObject
import KeyBind
import Internal.Texture (noTexture)

-- Player GameObject MoveSpeed
data Player = Player
  { object :: GameObject
  , moveSpeed :: Complex GLfloat
  } deriving (Eq)

instance HaveGameObject Player where
  gameObject (Player {object = object}) = object

update :: Keyset -> Player -> Player
update key player@( Player obj@(GameObject { pos = pos })
                           moveSpeed ) =
  player {object = obj {pos = crop $ pos + dpos}}
    where
      dpos = moveSpeed * (normalize . keysetToXY) key
      crop (x:+y) = (cx x :+ cy y)
      cx x | x < 20    = 20
           | x > 780   = 780
           | otherwise = x
      cy y | y < 20    = 20
           | y > 580   = 580
           | otherwise = y

normalize (x:+y) = case sqrt (x**2 + y**2) of
                     0 -> 0
                     n -> (x/n :+ y/n)

player = Player (GameObject (400:+400) 0 (16:+16) noTexture 0) 4
