module GameStage
  ( GameStage
  , gameStage
  ) where

import Data.Set

import qualified Class.GameScene as GS
import Class.Sprite
import KeyBind
import GlobalValue

import GameStage.GameObject
import qualified GameStage.Player as P
import qualified GameStage.Bullet as B

data GameStage = GameStage
  { player :: P.Player
  , playerBullets :: [B.Bullet]
  } deriving Eq

instance GS.GameScene GameStage where
  update (GV {keyset = key})
         scene@(GameStage { player = player
                          }) = do
    case member QUIT key of
      True  -> return GS.EndScene
      False -> return $ GS.Replace $ ( shoot
                                     . update
                                     ) scene
        where
          update :: GameStage -> GameStage
          update (GameStage p pbs) = GameStage (P.update key p)
                                               (Prelude.map B.update pbs)
          shoot :: GameStage -> GameStage
          shoot stage@(GameStage { player = p
                                 , playerBullets = pbs
                                 })
            = let ppos = (pos.gameObject) p
              in stage { playerBullets = if member A key
                                           then B.playerBullet ppos : pbs
                                           else pbs }

  render (GameStage { player = player
                    , playerBullets = playerBullets
                    }) = do
    render $ gameObject player
    mapM_ (render.gameObject) playerBullets
    return ()

gameStage :: IO GameStage
gameStage = return $ GameStage P.player []
