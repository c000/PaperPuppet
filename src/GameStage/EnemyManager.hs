module GameStage.EnemyManager
  ( EnemyList
  , spawnEnemy
  , enemies
  ) where

import Data.Complex

import GlobalSettings
import GameStage.Enemy
import GameStage.GameObject

type Time = Integer
type EnemyList = [(Time, Enemy)]

spawnEnemy :: Time -> EnemyList -> ([Enemy], EnemyList)
spawnEnemy t el = (enemies, newEl)
  where
    (spawnEl, newEl) = span (\(st,_) -> st <= t) el
    enemies = map snd spawnEl

enemies :: EnemyList
enemies = s1 <*> mirror s1
  where
    s1 = do
      t <- [0,10..]
      return $ (,) t $ (enemy (800:+200)) {
         act = [((-3):+y, []) | y <- [0,0.01..]]
      }
    [] <*> es = es
    es <*> [] = es
    (e1:es1) <*> (e2:es2)
      = if fst e1 < fst e2
          then e1 : ( es1 <*> (e2:es2) )
          else e2 : ( (e1:es1) <*> es2 )

delay :: Time -> EnemyList -> EnemyList
delay time = map f
  where
    f (t, e) = (t + time, e)

mirror :: EnemyList -> EnemyList
mirror el = map mirror' el
  where
    mirror' (t, e) = (t, f e)
    f e@Enemy { object = g@GameObject { pos = p }
              , act = a
              }
      = e { object = g { pos = mirrorPos p }
          , act = map (\(p,b) -> (mirrorV p, b)) a
          }
    mirrorV (x:+y) = (x :+ (-y))
    hy = realToFrac windowHeight / 2
    mirrorPos (x:+y) = (x :+ (((y-hy)*(-1))+hy) )
