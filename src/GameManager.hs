{-# LANGUAGE ExistentialQuantification #-}

module GameManager
  ( SceneObject (..)
  , runGame
  , module GlobalValue
  ) where

import qualified Data.Set as S
import Graphics.UI.SDL (glSwapBuffers)
import Graphics.Rendering.OpenGL hiding (Replace)

import FPSManager
import KeyBind
import GlobalValue
import Class.GameScene

data SceneObject = forall a. GameScene a => SceneObject a

type SceneStack = [SceneObject]

runGame :: GlobalValue -> SceneStack -> IO ()
runGame _ [] = return ()
runGame gv@(GV {keyset = k}) stack@((SceneObject x):xs) = do
  clear [ColorBuffer]
  renderGame stack
  glSwapBuffers
  waitGame
  k <- updateKeyset k
  let newGV = gv {keyset = k}
  newScene <- update newGV x
  case newScene of
    Replace g -> runGame newGV ((SceneObject g):xs)
    AddScene g -> runGameStart (gv {keyset = S.empty}) ((SceneObject g):(SceneObject x):xs)
    EndScene -> runGameStart (gv {keyset = S.empty}) xs
    RemoveScenes i -> runGameStart (gv {keyset = S.empty}) (drop i xs)

runGameStart gv stack = do
  case stack of
    (SceneObject x):_ -> start gv x
    otherwise         -> return ()
  runGame gv stack

renderGame :: SceneStack -> IO ()
renderGame [] = return ()
renderGame ((SceneObject x):xs) = do
  if transparent x then renderGame xs
                   else return ()
  render x
