module GameStage.Enemy
  where

import Data.Complex

import GameStage.GameObject
import Internal.Texture

data Enemy = Enemy
  { object :: GameObject
  , life :: Int
  } deriving (Eq)

instance HaveGameObject Enemy where
  gameObject (Enemy {object = x}) = x

update :: Enemy -> Enemy
update enemy = newEnemy
  where
    g@(GameObject {pos = p}) = gameObject enemy
    newEnemy = enemy{ object = g {pos = p + ((-1):+0)} }

enemy pos = Enemy (GameObject pos 0 (16:+16) noTexture 0) 10
