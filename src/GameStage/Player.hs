module GameStage.Player
  ( Player (..)
  , player
  , update
  ) where

import Graphics.Rendering.OpenGL (GLfloat)
import Data.Complex
import GameStage.GameObject
import KeyBind
import Internal.Texture
import qualified GameStage.Bullet as B

-- Player GameObject MoveSpeed
data Player = Player
  { object :: GameObject
  , moveSpeed :: Complex GLfloat
  , shootSpan :: Int
  } deriving (Eq)

instance HaveGameObject Player where
  gameObject (Player {object = object}) = object

update :: Keyset -> Player -> Player
update key player@( Player obj@(GameObject { pos = pos })
                           moveSpeed
                           _ ) =
  player {object = obj {pos = crop $ pos + dpos}}
    where
      dpos = moveSpeed * (normalize . keysetToXY) key
      crop (x:+y) = (cx x :+ cy y)
      cx x | x < 100   = 100
           | x > 700   = 700
           | otherwise = x
      cy y | y < 100   = 100
           | y > 500   = 500
           | otherwise = y

normalize (x:+y) = case sqrt (x**2 + y**2) of
                     0 -> 0
                     n -> (x/n :+ y/n)

shoot :: Bool -> Player -> (Maybe B.BulletType, Player)
shoot trig p@Player { object = o
                    , shootSpan = span
                    }
  = (newB, newP)
  where
    newB = Just B.Normal
    newP = p { shootSpan = if trig then span+1 else 0 }

player = do
  t <- loadTexture "res/player.png"
  let ta = TA t (1,1) [(0,0)]
  return $ Player (defaultGameObject
                     { pos = 400 :+ 400
                     , radius = 0
                     , size = 70 :+ 600
                     , gameTexture = Just ta
                     , offset = 0 :+ (-255)
                     }) 4 0
