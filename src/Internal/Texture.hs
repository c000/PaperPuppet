module Internal.Texture
  where

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.Rendering.OpenGL
import System.IO.Unsafe

lena :: TextureObject
lena = unsafePerformIO $ do
  surface <- load "res/lena_std.tif"
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
