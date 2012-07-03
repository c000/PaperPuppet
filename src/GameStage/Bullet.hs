module GameStage.Bullet
  where

import Control.Applicative
import Data.Complex
import Data.Unique
import qualified Data.Map as M

import qualified Class.Sprite as SP
import GlobalSettings
import Internal.Texture
import GameStage.GameObject

data BulletType = Normal
  deriving Eq

data Bullet = Bullet
  { object :: GameObject
  , direction :: Pos
  } deriving (Eq)

instance HaveGameObject Bullet where
  gameObject (Bullet {object = x}) = x

update :: Bullet -> Maybe Bullet
update bullet = case newBullet of
                  nb@(Bullet { object = GameObject { pos = p } } )
                    | scrollOut p 32 -> Nothing
                    | otherwise      -> Just nb
  where
    Bullet {object = obj, direction = dp} = bullet
    p = pos obj
    scrollOut (x:+y) l = or [ x < -l
                            , x > realToFrac windowWidth + l
                            , y < -l
                            , y > realToFrac windowHeight + l
                            ]
    newBullet = bullet { object = obj { pos = p + dp } }

data PlayerBullets = PB
  { container :: M.Map Unique Bullet
  , defaultObject :: GameObject
  }

dispose (PB _ pb) = do
  freeGameObject pb


playerBullets = do
  PB <$> pure M.empty
     <*> loadBullet
  where
    loadBullet = do
      t <- loadTexture "res/playerBullet.png"
      return $ defaultGameObject { radius = 10
                                 , size = 30 :+ 30
                                 , gameTexture = Just $ TA t (1,1) [(0,0)]
                                 }

instance SP.Renderable PlayerBullets where
  render (PB m _) = mapM_ (SP.render.gameObject) $ M.elems m

spawnPB :: BulletType -> Pos -> PlayerBullets -> IO PlayerBullets
spawnPB bType pos pbs@(PB c obj)
  = case bType of
      Normal -> do
        let newB = Bullet (obj { pos = pos }) (9:+0)
        newC <- M.insert <$> newUnique
                         <*> pure newB
                         <*> pure c
        return $ pbs { container = newC }

updatePB :: PlayerBullets -> PlayerBullets
updatePB pb@(PB c _) = pb { container = M.mapMaybe update c }

enemyBullet direction pos = Bullet (defaultGameObject
                                      { pos = pos
                                      ,size = 8:+8
                                      }) direction
