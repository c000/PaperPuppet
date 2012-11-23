module GameStage.Types
    ( GameStage (..)
    ) where

import Data.Map as M
import Data.Unique

import GameStage.Player as P
import GameStage.Bullet as B
import GameStage.Enemy as E
import GameStage.EnemyManager as EM
import GameStage.BGManager as BG

data GameStage = GameStage
  { player :: P.Player
  , playerBullets :: B.PlayerBullets
  , enemies :: M.Map Unique E.Enemy
  , enemyList :: EM.EnemyList
  , enemyBullets :: M.Map Unique B.Bullet
  , bgStruct :: BG.BGStruct
  , time :: Integer
  }

