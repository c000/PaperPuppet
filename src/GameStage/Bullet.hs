module GameStage.Bullet
  where

import Data.Complex

import GameStage.GameObject
import Internal.Texture

data Bullet = Bullet
  { object :: GameObject
  } deriving (Eq)

instance HaveGameObject Bullet where
  gameObject (Bullet {object = x}) = x

update :: Bullet -> Bullet
update bullet = bullet { object = newObject }
  where
    Bullet {object = currentObject} = bullet
    newObject = objUpdate currentObject
    objUpdate now@(GameObject { pos = pos })
      = now {pos = pos + (1:+0)}

playerBullet pos = Bullet (GameObject pos 8 (8:+8) noTexture 0)
