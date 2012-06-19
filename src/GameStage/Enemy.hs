module GameStage.Enemy
  where

import Data.Complex

import Graphics.Rendering.OpenGL (GLfloat)

import GlobalSettings
import GameStage.GameObject
import Internal.Texture

data Enemy = Enemy
  { object :: GameObject
  , life :: Int
  , move :: [Complex GLfloat]
  } deriving (Eq)

instance HaveGameObject Enemy where
  gameObject (Enemy {object = x}) = x

scrollOutLength = 32

update :: Enemy -> Maybe Enemy
update enemy = case newEnemy of
                 Nothing -> Nothing
                 Just ne@(Enemy { object = GameObject { pos = p } } )
                   | scrollOut p -> Nothing
                   | otherwise   -> Just ne
  where
    g@(GameObject {pos = p}) = gameObject enemy
    scrollOut (x:+y) = or [ x < -scrollOutLength
                          , x > realToFrac windowWidth + scrollOutLength
                          , y < -scrollOutLength
                          , y > realToFrac windowWidth + scrollOutLength
                          ]
    newEnemy = case move enemy of
      []   -> Nothing
      m:ms -> Just enemy { object = g {pos = p + m}
                         , move = ms
                         }

enemy pos = Enemy (GameObject pos 8 (16:+16) noTexture 0) 10 (repeat $ (-1):+0)
