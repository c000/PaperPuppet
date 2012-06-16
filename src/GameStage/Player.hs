module GameStage.Player
  ( Player (..)
  , player
  , update
  ) where

import Graphics.Rendering.OpenGL (GLfloat)
import Data.Complex
import GameStage.GameObject
import KeyBind

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
      dpos = moveSpeed * keysetToXY key
      crop (x:+y) = (cx x :+ cy y)
      cx x | x < 20    = 20
           | x > 780   = 780
           | otherwise = x
      cy y | y < 20    = 20
           | y > 580   = 580
           | otherwise = y

player = Player (GameObject (400:+400) 0 (16:+16)) 10
