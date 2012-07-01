module GameStage
  ( GameStage
  , gameStage
  ) where

import Control.Applicative
import Control.Monad
import Data.Set
import qualified Data.Map as M
import Data.Unique

import qualified Class.GameScene as GS
import Class.Sprite
import KeyBind
import GlobalValue

import GameStage.GameObject
import qualified GameStage.Player as P
import qualified GameStage.Bullet as B
import qualified GameStage.Enemy as E
import qualified GameStage.EnemyManager as EM
import GameStage.BGManager as BG
import GameStage.Collider

data GameStage = GameStage
  { player :: P.Player
  , playerBullets :: M.Map Unique B.Bullet
  , enemies :: M.Map Unique E.Enemy
  , enemyList :: EM.EnemyList
  , enemyBullets :: M.Map Unique B.Bullet
  , bgStruct :: BG.BGStruct
  , time :: Integer
  }

instance GS.GameScene GameStage where
  update (GV {keyset = key}) scene = do
    case member QUIT key of
      True  -> return GS.EndScene
      False -> GS.Replace <$> do
        ( update >=> shoot >=> spawnEnemy >=> hitEnemy >=> shootEnemy ) scene
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
      update (GameStage p pbs es el ebs bgs time)
        = return $ GameStage (P.update key p)
                             (M.mapMaybe B.update pbs)
                             (M.mapMaybe E.update es)
                             el
                             (M.mapMaybe B.update ebs)
                             (BG.update bgs)
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
      shootEnemy :: GameStage -> IO GameStage
      shootEnemy stage@GameStage { enemies = es
                                 , enemyBullets = ebs
                                 }
        = do
             let newB = concatMap E.getBullets (M.elems es)
             nebs <- mapM (\x -> (,) <$> newUnique <*> pure x) newB
             return $ stage { enemyBullets = Prelude.foldl
                                               ((flip . uncurry) M.insert)
                                               ebs
                                               nebs
                            }

  render (GameStage { player = p
                    , playerBullets = pbs
                    , enemies = es
                    , enemyBullets = ebs
                    , bgStruct = bgs
                    }) = do
    render $ gameObject p
    mapM_ (render.gameObject) $ M.elems pbs
    mapM_ (render.gameObject) $ M.elems es
    mapM_ (render.gameObject) $ M.elems ebs
    BG.renderRim bgs
    return ()

  dispose GameStage { bgStruct = bgs
                    }
    = do BG.dispose bgs

gameStage :: IO GameStage
gameStage = do
  bgs <- BG.load
  return $ GameStage P.player M.empty M.empty EM.constEnemy M.empty bgs 0
