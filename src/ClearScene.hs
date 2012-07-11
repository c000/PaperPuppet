module ClearScene
  ( clearScene
  ) where

import Data.Set

import Class.GameScene as GS
import GlobalValue
import KeyBind

data ClearScene = ClearScene Int

instance GameScene ClearScene where
  update (GV {keyset = key}) scene
    | member A key    = return $ GS.EndScene
    | member QUIT key = return $ GS.EndScene
    | otherwise       = return $ GS.Replace scene


clearScene :: Int -> IO ClearScene
clearScene score = return $ ClearScene score
