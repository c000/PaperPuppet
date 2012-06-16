module Initialize
  ( withGameInit
  ) where

import Graphics.UI.SDL as SDL
import Graphics.Rendering.OpenGL

import GlobalSettings

withGameInit :: (Surface -> IO ()) -> IO ()
withGameInit f = do
  withInit [InitEverything] $ do
    window <- setVideoMode windowWidth windowHeight windowDepth [OpenGL]
    showCursor False
    initOpenGL
    f window

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
