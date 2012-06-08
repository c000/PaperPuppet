module GameStage.Player
  ( Player (..)
  , player
  , update
  ) where

import Data.Complex
import GameStage.GameObject
import KeyBind

type Player = GameObject

update :: Keyset -> Player -> Player
update key player@(GameObject { pos = pos }) =
  player { pos = crop $ pos + dpos}
    where
      dpos = moveSpeed * keysetToXY key
      crop (x:+y) = (cx x :+ cy y)
      cx x | x < 20    = 20
           | x > 780   = 780
           | otherwise = x
      cy y | y < 20    = 20
           | y > 580   = 580
           | otherwise = y

moveSpeed = 10

player = GameObject (400:+400) 0 (16:+16)
