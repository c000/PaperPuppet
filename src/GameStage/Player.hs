module GameStage.Player
  ( Player (..)
  , player
  , update
  , render
  , hit
  , gameOver
  , shoot
  ) where

import Graphics.Rendering.OpenGL (GLfloat)
import Data.Complex
import GameStage.GameObject
import KeyBind
import qualified Class.Sprite as SP
import Internal.Texture
import qualified GameStage.Bullet as B

data State = Normal | Invincible Int | Dead Int
  deriving Eq

data Player = Player
  { object :: GameObject
  , state :: State
  , age :: Int
  , moveSpeed :: Complex GLfloat
  , shootSpan :: Int
  , remaining :: Int
  } deriving (Eq)

instance HaveGameObject Player where
  gameObject (Player {object = object}) = object

spawnPoint = 200 :+ 300

update :: Keyset -> Player -> Player
update key player@( Player obj@(GameObject { pos = pos })
                           s
                           a
                           moveSpeed
                           _ _) =
  player { object = obj {pos = newPos}
         , state = newS
         , age = a + 1
         }
    where
      newPos = case s of
                 Dead _ -> spawnPoint
                 _      -> crop $ pos + dpos
      dpos = moveSpeed * (normalize . keysetToXY) key
      crop (x:+y) = (cx x :+ cy y)
      cx x | x < 100   = 100
           | x > 700   = 700
           | otherwise = x
      cy y | y < 100   = 100
           | y > 500   = 500
           | otherwise = y
      newS = case s of
               Dead i | i <= 0    -> Invincible 120
                      | otherwise -> Dead (i-1)
               Invincible i | i <= 0    -> Normal
                            | otherwise -> Invincible (i-1)
               x -> x

normalize (x:+y) = case sqrt (x**2 + y**2) of
                     0 -> 0
                     n -> (x/n :+ y/n)

hit :: Bool -> Player -> Player
hit True p@Player { remaining = r }
  = case state p of
      Normal -> p { state = Dead (if r == 0 then maxBound else 120)
                  , age = 0
                  , remaining = r - 1
                  }
      _ -> p
hit _ p = p

gameOver :: Player -> Bool
gameOver Player { remaining = r }
  = r == 0

render :: Player -> IO ()
render p = do
  case state p of
    Dead   _ -> return ()
    Normal -> SP.render $ object p
    Invincible _ | age p `mod` 2 == 0 -> SP.render $ object p
                 | otherwise          -> return ()

shoot :: Bool -> Player -> (Maybe B.BulletType, Player)
shoot trig p@Player { shootSpan = span
                    }
  = (newB, newP)
  where
    t = case state p of
          Dead _ -> False
          _      -> trig
    newB = case t of
             False -> Nothing
             True | span `mod` 4 == 0 -> Just B.Normal
                  | otherwise         -> Nothing
    newP = p { shootSpan = if t then span+1 else 0 }

player = do
  t <- loadTexture "res/player.png"
  let ta = TA t (1,1) [(0,0)]
  return $ Player (defaultGameObject
                     { pos = spawnPoint
                     , radius = 2
                     , size = 70 :+ 600
                     , gameTexture = Just ta
                     , offset = 0 :+ (-255)
                     }) Normal 0 4 0 4
