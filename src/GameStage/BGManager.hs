module GameStage.BGManager
  where

import Control.Applicative
import Data.Array.IArray
import Data.Complex

import qualified Class.Sprite as SP
import GlobalSettings
import Internal.Texture
import GameStage.GameObject

-- BGState BGnumber
data BGState = Stay Int
             | Move Int

type AgeNum = Int

data BGStruct = BGS
  { rim :: GameObject
  , backgrounds :: Array Int GameObject
  , bgSequence :: [(AgeNum, BGState)]
  , age :: AgeNum
  }

update bgs@BGS { bgSequence = bgSeq
               , age = a
               }
  = do let newBgSeq = case bgSeq of
             now : next : bs
               | (fst next) <= a -> next : bs
               | otherwise       -> now : next : bs
             other -> other
       bgs { bgSequence = newBgSeq
           , age = a+1
           }

render struct@BGS { backgrounds = bgs
                  , bgSequence = bgSeq
                  , age = a
                  }
  = case bgSeq of
      [] -> return ()
      (_, Stay num):_ -> SP.render $ bgs ! num
      [(_, Move num)] -> SP.render $ bgs ! num
      (t0, Move num):(t1, _):_ -> do
        let prop = (i2f t1 - i2f t0) / (i2f a - i2f t0)
            p = (-prop * i2f windowWidth) :+ 0
        render $ struct { bgSequence = tail bgSeq }
        SP.render $ (bgs ! num) { pos = p }
  where
    i2f = fromIntegral

renderRim BGS { rim = r }
  = SP.render r

load :: IO BGStruct
load = BGS <$> loadRim
           <*> loadBGs
           <*> pure [(0, Stay 0)]
           <*> pure 0
  where
    windowSizeObject tex =
      defaultGameObject { size = fromIntegral windowWidth :+ fromIntegral windowHeight
                        , gameTexture = Just $ TA tex (1,1) [(0,0)]
                        }
    loadBGs = do
      l <- mapM loadTexture ["res/bg0.png"]
      let ix = length l - 1
      return $ listArray (0,ix) $ map windowSizeObject l
    loadRim = do
      t <- loadTexture "res/rim.png"
      return $ windowSizeObject t

dispose :: BGStruct -> IO ()
dispose (BGS r bgs _ _) = do
  freeGameObject r
  mapM_ freeGameObject $ elems bgs
