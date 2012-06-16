module KeyBind
  ( Key (..)
  , Keyset
  , updateKeyset
  , keysetToXY
  ) where

import Data.Complex
import Data.Set
import Graphics.UI.SDL
import Graphics.UI.SDL.Keysym

data Key = A | B | C | RB | LB | UB | DB | QUIT
  deriving (Eq, Ord, Show)

type Keyset = Set Key

eventToKey :: Event -> Keyset -> Keyset
eventToKey Quit = insert QUIT
eventToKey (KeyDown k) = normalKey insert k
eventToKey (KeyUp k) = normalKey delete k
eventToKey _ = id

normalKey :: (Key -> Keyset -> Keyset) -> Keysym -> Keyset -> Keyset
normalKey f (Keysym {symKey = k})
  | k == SDLK_z     = f A
  | k == SDLK_x     = f B
  | k == SDLK_c     = f C
  | k == SDLK_RIGHT = f RB
  | k == SDLK_LEFT  = f LB
  | k == SDLK_UP    = f UB
  | k == SDLK_DOWN  = f DB
  | k == SDLK_ESCAPE= f QUIT
  | otherwise       = id

updateKeyset :: Keyset -> IO Keyset
updateKeyset k = do
  event <- pollEvent
  case event of
    NoEvent -> return k
    some    -> updateKeyset (eventToKey some k)

keysetToXY :: (RealFloat a) => Keyset -> Complex a
keysetToXY k = (right - left) :+ (up - down)
  where
    right = b2i $ member RB k
    left  = b2i $ member LB k
    up    = b2i $ member UB k
    down  = b2i $ member DB k
    b2i False = 0
    b2i True  = 1
