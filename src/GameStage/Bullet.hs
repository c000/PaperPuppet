module GameStage.Bullet
  where

import Data.Complex

import GameStage.GameObject
import Internal.Texture

data Bullet = Bullet
  { object :: GameObject
  , direction :: Pos
  } deriving (Eq)

instance HaveGameObject Bullet where
  gameObject (Bullet {object = x}) = x

update :: Bullet -> Bullet
update bullet = bullet { object = newObject }
  where
    Bullet {object = currentObject, direction = dp} = bullet
    newObject = objUpdate currentObject
    objUpdate now@(GameObject { pos = p })
      = now {pos = p + dp}

playerBullet pos = Bullet (GameObject pos 9 (8:+8) noTexture 0) (9:+0)
