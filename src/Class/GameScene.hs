{-# LANGUAGE ExistentialQuantification #-}

module Class.GameScene
  where

import qualified KeyBind as K

data Result = forall a. GameScene a => Replace a
            | forall a. GameScene a => AddScene a
            | EndScene

class GameScene a where
  update :: K.Keyset -> a -> IO Result

  render :: a -> IO ()
  render _ = return ()

  transparent :: a -> Bool
  transparent _ = False
