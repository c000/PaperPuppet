module Internal.OpenGL
  where

import Graphics.Rendering.OpenGL

v2 :: GLfloat -> GLfloat -> IO ()
v2 x y = vertex (Vertex2 x y)

t2 :: GLfloat -> GLfloat -> IO ()
t2 x y = texCoord (TexCoord2 x y)

c3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
c3 r g b = color $ Color3 r g b
