module GameStage
  ( GameStage
  , gameStage
  ) where

import Control.Applicative
import Control.Monad
import Data.Set
import qualified Data.Map as M
import Data.Unique
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Class.GameScene as GS
import Class.Sprite
import KeyBind
import GlobalValue

import GameStage.GameObject
import qualified GameStage.Player as P
import qualified GameStage.Bullet as B
import qualified GameStage.Enemy as E
import qualified GameStage.EnemyManager as EM
import GameStage.Collider

data GameStage = GameStage
  { player :: P.Player
  , playerBullets :: M.Map Unique B.Bullet
  , enemies :: M.Map Unique E.Enemy
  , enemyList :: EM.EnemyList
  , time :: Integer
  } deriving Eq

instance GS.GameScene GameStage where
  update (GV {keyset = key}) scene = do
    case member QUIT key of
      True  -> return GS.EndScene
      False -> GS.Replace <$> do
        ( update >=> shoot >=> spawnEnemy >=> hitEnemy ) scene
    where
      hitEnemy stage@(GameStage { playerBullets = pbs
                                , enemies = es
                                })
        = do let list = collide pbs es
                 kpbs = Prelude.map fst list
                 kes  = Prelude.map snd list
             return stage { playerBullets = Prelude.foldl
                                              (flip M.delete)
                                              pbs
                                              kpbs
                          , enemies = Prelude.foldl
                                        (flip M.delete)
                                        es
                                        kes
                          }
      spawnEnemy stage@(GameStage { enemies = es
                                  , enemyList = el
                                  , time = t
                                  })
        = do let (newEs, newEl) = EM.spawnEnemy t el
             nes <- mapM (\x -> (,) <$> newUnique <*> pure x) newEs
             return $ stage { enemies = Prelude.foldl
                                          ((flip . uncurry) M.insert)
                                          es
                                          nes
                            , enemyList = newEl
                            }
      update :: GameStage -> IO GameStage
      update (GameStage p pbs es el time)
        = return $ GameStage (P.update key p)
                             (M.mapMaybe B.update pbs)
                             (M.mapMaybe E.update es)
                             el
                             (time + 1)
      shoot :: GameStage -> IO GameStage
      shoot stage@(GameStage { player = p
                             , playerBullets = pbs
                             })
        = do let ppos = (pos.gameObject) p
                 newB = B.playerBullet ppos
             newPbs <- if member A key
                         then M.insert <$> newUnique
                                       <*> pure newB
                                       <*> pure pbs
                         else return pbs
             return $ stage { playerBullets = newPbs }

  render (GameStage { player = p
                    , playerBullets = pbs
                    , enemies = es
                    }) = do
    render $ gameObject p
    mapM_ (render.gameObject) $ M.elems pbs
    mapM_ (render.gameObject) $ M.elems es
    return ()

gameStage :: IO GameStage
gameStage = return $ GameStage P.player M.empty M.empty EM.constEnemy 0
