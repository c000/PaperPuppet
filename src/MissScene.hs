module MissScene
  ( missScene
  ) where

import Data.Set

import Class.GameScene as GS
import GlobalValue
import KeyBind

data MissScene = MissScene

instance GameScene MissScene where
  update (GV {keyset = key}) scene
    | member A key    = return $ GS.EndScene
    | member QUIT key = return $ GS.EndScene
    | otherwise       = return $ GS.Replace scene

missScene :: IO MissScene
missScene = return MissScene
