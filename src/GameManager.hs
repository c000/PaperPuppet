{-# LANGUAGE ExistentialQuantification #-}

module GameManager
  where

import Graphics.UI.SDL (glSwapBuffers)

import KeyBind
import Class.GameScene

data Object = forall a. GameScene a => Object a

type SceneStack = [Object]

runGame :: Keyset -> SceneStack -> IO ()
runGame _ [] = return ()
runGame k ((Object x):xs) = do
  render x
  glSwapBuffers
  k <- updateKeyset k
  newScene <- update k x
  case newScene of
    Replace g -> runGame k ((Object g):xs)
    AddScene g -> runGame k ((Object g):(Object x):xs)
    EndScene -> runGame k xs
