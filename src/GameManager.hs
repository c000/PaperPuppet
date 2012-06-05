{-# LANGUAGE ExistentialQuantification #-}

module GameManager
  ( Object (..)
  , runGame
  , module GlobalValue
  ) where

import qualified Data.Set as S
import Graphics.UI.SDL (glSwapBuffers)
import Graphics.Rendering.OpenGL hiding (Replace)

import KeyBind
import GlobalValue
import Class.GameScene

data Object = forall a. GameScene a => Object a

type SceneStack = [Object]

runGame :: GlobalValue -> SceneStack -> IO ()
runGame _ [] = return ()
runGame gv@(GV {keyset = k}) stack@((Object x):xs) = do
  clear [ColorBuffer]
  renderGame stack
  glSwapBuffers
  k <- updateKeyset k
  let newGV = gv {keyset = k}
  newScene <- update newGV x
  case newScene of
    Replace g -> runGame newGV ((Object g):xs)
    AddScene g -> runGame (gv {keyset = S.empty}) ((Object g):(Object x):xs)
    EndScene -> runGame (gv {keyset = S.empty}) xs
    RemoveScenes i -> runGame (gv {keyset = S.empty}) (drop i xs)

renderGame :: SceneStack -> IO ()
renderGame [] = return ()
renderGame ((Object x):xs) = do
  if transparent x then renderGame xs
                   else return ()
  render x
