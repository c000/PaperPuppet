module GameStage.Bullet
  where

import Data.Complex

import GlobalSettings
import GameStage.GameObject

data Bullet = Bullet
  { object :: GameObject
  , direction :: Pos
  } deriving (Eq)

instance HaveGameObject Bullet where
  gameObject (Bullet {object = x}) = x

update :: Bullet -> Maybe Bullet
update bullet = case newBullet of
                  nb@(Bullet { object = GameObject { pos = p } } )
                    | scrollOut p 32 -> Nothing
                    | otherwise      -> Just nb
  where
    Bullet {object = obj, direction = dp} = bullet
    p = pos obj
    scrollOut (x:+y) l = or [ x < -l
                            , x > realToFrac windowWidth + l
                            , y < -l
                            , y > realToFrac windowHeight + l
                            ]
    newBullet = bullet { object = obj { pos = p + dp } }

playerBullet pos = Bullet (defaultGameObject
                             { pos = pos
                             , radius = 5
                             , size = 8:+8
                             }) (9:+0)
enemyBullet direction pos = Bullet (defaultGameObject
                                      { pos = pos
                                      ,size = 8:+8
                                      }) direction
