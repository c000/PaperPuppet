module GameStage
  ( GameStage
  , gameStage
  ) where

import Data.Set
import Data.Complex

import qualified Class.GameScene as GS
import Class.Sprite
import KeyBind
import GlobalValue

import GameStage.GameObject
import qualified GameStage.Player as P

data GameStage = GameStage
  { player :: P.Player
  } deriving Eq

instance GS.GameScene GameStage where
  update (GV {keyset = key}) scene@(GameStage {player = player}) = do
    case member QUIT key of
      True  -> return GS.EndScene
      False -> return $ GS.Replace $ scene {player = P.update key player}

  render (GameStage obj) = do
    render obj
    return ()

gameStage :: IO GameStage
gameStage = return $ GameStage P.player
