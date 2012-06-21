module Initialize
  ( withGameInit
  ) where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer
import Graphics.Rendering.OpenGL

import GlobalSettings

withGameInit :: (Surface -> IO ()) -> IO ()
withGameInit f = do
  withInit [InitEverything] $ do
    window <- setVideoMode windowWidth windowHeight windowDepth [OpenGL]
    showCursor False
    openAudio 44100 AudioS16Sys 2 4096
    initOpenGL
    f window
    closeAudio

initOpenGL :: IO ()
initOpenGL = do
  mapM_ ($= Enabled) [ texture Texture2D
                     , blend
                     ]
  matrixMode $= Projection
  ortho 0 w 0 h (-1) 1
  matrixMode $= Modelview 0
    where
      w = realToFrac windowWidth
      h = realToFrac windowHeight
