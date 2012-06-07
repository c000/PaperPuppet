module Internal.Texture
  where

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.Rendering.OpenGL
import System.IO.Unsafe

loadTexturePow2 :: FilePath -> TextureObject
loadTexturePow2 fileName = unsafePerformIO $ do
  surface <- load fileName
  let w = fromIntegral $ surfaceGetWidth surface
      h = fromIntegral $ surfaceGetHeight surface
  pixels <- surfaceGetPixels surface
  let pixelData = PixelData RGBA UnsignedByte pixels
  [tex] <- genObjectNames 1
  textureBinding Texture2D $= Just tex
  textureFilter Texture2D $=! ((Linear', Nothing), Linear')
  texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D w h) 0 pixelData
  freeSurface surface
  return tex
