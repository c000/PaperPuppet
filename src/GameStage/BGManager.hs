module GameStage.BGManager
  where

import Control.Applicative
import Data.Complex

import Class.Sprite
import GlobalSettings
import Internal.Texture
import GameStage.GameObject

data BGStruct = BGS
  { rim :: GameObject
  , age :: Int
  }

update bgs@BGS { age = a
               }
  = bgs { age = a+1
        }


renderRim BGS { rim = r }
  = render r

load :: IO BGStruct
load = BGS <$> loadRim
           <*> pure 0
  where
    windowSizeObject tex =
      defaultGameObject { size = fromIntegral windowWidth :+ fromIntegral windowHeight
                        , gameTexture = Just $ TA tex (1,1) [(0,0)]
                        }
    loadRim = do
      t <- loadTexture "res/rim.png"
      return $ windowSizeObject t

dispose :: BGStruct -> IO ()
dispose (BGS r _) = do
  freeGameObject r
