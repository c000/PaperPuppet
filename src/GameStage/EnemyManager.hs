module GameStage.EnemyManager
  where

import Data.Complex
import Data.List

import GameStage.Enemy

type Time = Integer
type EnemyList = [(Time, Enemy)]

spawnEnemy :: Time -> EnemyList -> ([Enemy], EnemyList)
spawnEnemy t el = (enemies, newEl)
  where
    (spawnEl, newEl) = span (\(st,_) -> st <= t) el
    enemies = map snd spawnEl

constEnemy = [(t, enemy (600:+300)) | t <- [0,60..]]

sortEnemyList :: EnemyList -> EnemyList
sortEnemyList = sortBy f
  where
    f (a,_) (b,_) = compare a b
