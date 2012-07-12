module GameStage
  ( GameStage
  , gameStage
  ) where

import Control.Applicative
import Control.Monad
import Data.Set
import qualified Data.Map as M
import qualified Data.List as L
import Data.Unique

import qualified Class.GameScene as GS
import Class.Sprite
import KeyBind
import GlobalValue

import qualified Sound as SO

import MissScene (missScene)
import ClearScene (clearScene)

import GameStage.GameObject
import qualified GameStage.Player as P
import qualified GameStage.Bullet as B
import qualified GameStage.Enemy as E
import qualified GameStage.EnemyManager as EM
import qualified GameStage.BGManager as BG
import GameStage.Collider

data GameStage = GameStage
  { player :: P.Player
  , playerBullets :: B.PlayerBullets
  , enemies :: M.Map Unique E.Enemy
  , enemyList :: EM.EnemyList
  , enemyBullets :: M.Map Unique B.Bullet
  , bgStruct :: BG.BGStruct
  , time :: Integer
  }

data GameOver = Continue | Miss | Clear

instance GS.GameScene GameStage where
  update gv@(GV {keyset = key}) scene = do
    case member QUIT key of
      True  -> return GS.EndScene
      False -> do
        newScene <- ( update >=> shoot >=> spawnEnemy >=> hitEnemy >=> hitPlayer >=> shootEnemy ) scene
        case gameOver newScene of
          Continue -> return $ GS.Replace newScene
          Miss     -> GS.dispose newScene >> GS.Replace <$> missScene
          Clear    -> GS.dispose newScene >> GS.Replace <$> clearScene 0
    where
      gameOver :: GameStage -> GameOver
      gameOver GameStage { player = p
                         , enemies = es
                         , enemyList = el
                         }
        | P.gameOver p = Miss
        | L.null el && M.null es = Clear
        | otherwise    = Continue
      hitPlayer stage@GameStage { player = p
                                , enemies = es
                                , enemyBullets = ebs
                                }
        = do let ds = (Prelude.map gameObject . M.elems) es ++
                      (Prelude.map gameObject . M.elems) ebs
                 hits = or $ Prelude.map (within (gameObject p)) ds
             return $ stage { player = P.hit hits p
                            }
      hitEnemy stage@(GameStage { playerBullets = pbs
                                , enemies = es
                                })
        = do let list = collide (B.container pbs) es
                 kpbs = Prelude.map fst list
                 kes  = Prelude.map snd list
             return stage { playerBullets = pbs { B.container = Prelude.foldl
                                                     (flip M.delete)
                                                     (B.container pbs)
                                                     kpbs
                                                }
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
                             (B.updatePB pbs)
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
                 (bt,newP) = P.shoot (member A key) p
             newPbs <- case bt of
               Nothing -> return pbs
               Just t  -> do
                 SO.writeChan (sound gv) (SO.Shoot)
                 B.spawnPB t ppos pbs
             return $ stage { player = newP
                            , playerBullets = newPbs
                            }
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
    BG.render bgs
    P.render p
    render pbs
    mapM_ (render.gameObject) $ M.elems es
    mapM_ (render.gameObject) $ M.elems ebs
    BG.renderRim bgs
    return ()

  dispose GameStage { bgStruct = bgs
                    }
    = do BG.dispose bgs

gameStage :: IO GameStage
gameStage = GameStage
    <$> P.player
    <*> B.playerBullets
    <*> pure M.empty
    <*> pure EM.enemies
    <*> pure M.empty
    <*> BG.load
    <*> pure 0
