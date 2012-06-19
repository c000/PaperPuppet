module GameStage.Collider
  where

import Prelude as P
import Data.List as L
import Data.Map as M
import Data.Unique

import Class.Sprite
import GameStage.GameObject

collide xs ys
  = case minViewWithKey xs of
      Nothing -> []
      Just ((kx, x), nextxs) ->
        let objys = toList ys
        in case find (\(ky, y) -> gameObject x `within` gameObject y) objys of
             Nothing -> collide nextxs ys
             Just (ky, _) -> (kx, ky) : collide nextxs (M.delete ky ys)
