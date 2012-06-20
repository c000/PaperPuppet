module GlobalValue where

import Control.Applicative
import qualified Data.Set as S
import Graphics.UI.SDL (Surface)

import KeyBind
import Sound

data GlobalValue = GV
  { keyset :: Keyset
  , window :: Surface
  , sound :: SoundChan
  }

defaultGV :: Surface -> IO GlobalValue
defaultGV w
  = GV <$> pure S.empty
       <*> pure w
       <*> getSoundThread
