module GameStage.Enemy
  where

import Data.Complex

import Graphics.Rendering.OpenGL (GLfloat)

import GlobalSettings
import GameStage.GameObject
import qualified GameStage.Bullet as B

data Enemy = Enemy
  { object :: GameObject
  , life :: Int
  , act :: [(Complex GLfloat, [Pos -> B.Bullet])]
  }

instance HaveGameObject Enemy where
  gameObject (Enemy {object = x}) = x

update :: Enemy -> Maybe Enemy
update enemy = case newEnemy of
                 Nothing -> Nothing
                 Just ne@(Enemy { object = GameObject { pos = p } } )
                   | scrollOut p 32 -> Nothing
                   | otherwise   -> Just ne
  where
    g@(GameObject {pos = p}) = gameObject enemy
    scrollOut (x:+y) l = or [ x < -l
                            , x > realToFrac windowWidth + l
                            , y < -l
                            , y > realToFrac windowHeight + l
                            ]
    newEnemy = case act enemy of
      []   -> Nothing
      (m,_):ms -> Just enemy { object = g {pos = p + m}
                             , act = ms
                             }

getBullets :: Enemy -> [B.Bullet]
getBullets enemy = bullets
  where
    p = pos $ gameObject enemy
    bullets = map (\f -> f p) $ (snd.head.act) enemy

enemy pos = Enemy (defaultGameObject { pos = pos
                                     , radius = 8
                                     , size = 16:+16
                                     }) 10
  ((repeat $ ((-1):+0)) `zip` bullets 0)
  where
    bullets 10 = [B.enemyBullet (0:+2)] : bullets 0
    bullets x  = [] : bullets (x+1)
