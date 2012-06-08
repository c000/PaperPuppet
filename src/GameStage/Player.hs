module GameStage.Player
  ( Player (..)
  , update
  ) where

import Data.Complex
import GameStage.GameObject
import KeyBind

type Player = GameObject

update :: Keyset -> Player -> Player
update key player@(GameObject { pos = pos }) = player { pos = pos + dpos}
  where
    dpos = moveSpeed * keysetToXY key

moveSpeed = 10
