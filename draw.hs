module Draw where

import Graphics.Rendering.OpenGL

import Vec


type RGB = (Float, Float, Float)

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

colorRGB :: RGB -> IO ()
colorRGB (r, g, b) = color3f (realToFrac r) (realToFrac g) (realToFrac b)

vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z

vertexVec :: Vec2 -> IO ()
vertexVec (Vec2 x y) = vertex3f (realToFrac x) (realToFrac y) 0

drawRect :: Float -> Float -> Vec2 -> IO ()
drawRect w h pos = renderPrimitive Quads vecs
    where vecs = mapM_ vertexVec corners
          offsets = [ (w, h)
                    , (-w, h)
                    , (-w, -h)
                    , (w, -h)
                    ]
          vecOffsets = map vec2 offsets
          corners = map (/+/ pos) vecOffsets

drawSquare :: Float -> Vec2 -> IO ()
drawSquare size = drawRect size size
