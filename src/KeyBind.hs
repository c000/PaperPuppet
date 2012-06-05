module KeyBind
  where

import Data.Set
import Graphics.UI.SDL

data Key = A | B | C | RB | LB | UB | DB | QUIT
  deriving (Eq, Ord, Show)

type Keyset = Set Key

eventToKey :: Event -> Keyset -> Keyset
eventToKey Quit = insert QUIT
eventToKey _ = id

updateKeyset :: Keyset -> IO Keyset
updateKeyset k = do
  event <- pollEvent
  case event of
    NoEvent -> return k
    some    -> updateKeyset (eventToKey some k)
