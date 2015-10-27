module Draw where

import Graphics.Rendering.OpenGL hiding (Rect)

import Vec

type RGB = (Float, Float, Float)

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

colorRGB :: RGB -> IO ()
colorRGB (r, g, b) = color3f (realToFrac r) (realToFrac g) (realToFrac b)

vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z

vertexVec :: Vec2f -> IO ()
vertexVec (Vec2 x y) = vertex3f (realToFrac x) (realToFrac y) 0

drawRect :: Rect -> IO ()
drawRect (Rect pos (Vec2 w h)) = renderPrimitive Quads vecs
    where vecs = mapM_ vertexVec corners
          offsets = [ (w, h)
                    , (-w, h)
                    , (-w, -h)
                    , (w, -h)
                    ]
          vecOffsets = map vec2 offsets
          corners = map (+ pos) vecOffsets

drawColorRect color rect = do
    colorRGB color
    drawRect rect
