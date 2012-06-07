module TitleScene
  ( TitleScene (..)
  , titleScene
  ) where

import Data.Set
import Graphics.Rendering.OpenGL

import Class.GameScene as GS
import Internal.Texture
import KeyBind
import GlobalValue
import GameStage

data TitleState = GameStart | GameEnd
  deriving (Eq, Enum, Bounded, Show)

next :: TitleState -> TitleState
next x
  | x == maxBound = minBound
  | otherwise     = succ x

prev :: TitleState -> TitleState
prev x
  | x == minBound = maxBound
  | otherwise     = pred x

data TitleScene = TitleScene
  { state :: TitleState
  , movable :: Bool
  } deriving Eq

instance GameScene TitleScene where
  update (GV {keyset = key}) title@(TitleScene st movable) = do
    case member QUIT key of
      True  -> return EndScene
      False -> return $ pressStart key $ moveCursor key title

  render (TitleScene st _) = do
    preservingMatrix $ do
      textureBinding Texture2D $=! (Just $ loadTexturePow2 "res/title.png")
      renderPrimitive Quads $ do
        c3 1 1 1
        t2 0 1
        v2 0 0
        t2 1 1
        v2 800 0
        t2 1 0
        v2 800 600
        t2 0 0
        v2 0 600
    preservingMatrix $ do
      textureBinding Texture2D $= Nothing
      blendFunc $=! (OneMinusDstColor, One)
      case st of
        GameStart -> translate (Vector3 10 110 0 :: Vector3 GLfloat)
        GameEnd   -> translate (Vector3 10 10 0 :: Vector3 GLfloat)
      renderPrimitive Quads $ do
        c3 0.5 0.5 0.5
        v2 0 0
        v2 100 0
        v2 100 100
        v2 0 100
    return ()

titleScene :: TitleScene
titleScene = TitleScene GameStart True

moveCursor :: Keyset -> TitleScene -> TitleScene
moveCursor key title@(TitleScene st movable)
  | member UB key = TitleScene nst False
  | member DB key = TitleScene pst False
  | otherwise     = TitleScene st  True
  where
    nst = if movable then next st else st
    pst = if movable then prev st else st

pressStart :: Keyset -> TitleScene -> Result
pressStart key title@(TitleScene st movable)
  | member A key = case st of
      GameStart -> AddScene gameStage
      GameEnd   -> EndScene
  | otherwise = GS.Replace title

v2 :: GLfloat -> GLfloat -> IO ()
v2 x y = vertex (Vertex2 x y)

t2 :: GLfloat -> GLfloat -> IO ()
t2 x y = texCoord (TexCoord2 x y)

c3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
c3 r g b = color $ Color3 r g b
