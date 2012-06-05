{-# LANGUAGE ExistentialQuantification #-}

module Class.GameScene
  where

import GlobalValue

data Result = forall a. GameScene a => Replace a
            | forall a. GameScene a => AddScene a
            | EndScene
            | RemoveScenes Int

class GameScene a where
  update :: GlobalValue -> a -> IO Result

  render :: a -> IO ()
  render _ = return ()

  transparent :: a -> Bool
  transparent _ = False
