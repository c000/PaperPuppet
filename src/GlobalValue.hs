module GlobalValue where

import qualified Data.Set as S
import Graphics.UI.SDL (Surface)

import KeyBind

data GlobalValue = GV
  { keyset :: Keyset
  , window :: Surface
  } deriving Show

defaultGV :: Surface -> GlobalValue
defaultGV w = GV S.empty w
