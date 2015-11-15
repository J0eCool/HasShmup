module Draw where

import Graphics.Rendering.OpenGL hiding (Rect, RGB)

import Rect
import Vec

data RGB = RGB Float Float Float

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

colorRGB :: RGB -> IO ()
colorRGB (RGB r g b) = color3f (realToFrac r) (realToFrac g) (realToFrac b)

vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z

vertexVec :: Vec2f -> IO ()
vertexVec (Vec2 x y) = vertex3f (realToFrac x) (realToFrac y) 0

drawRectBoundedBy :: Vec2f -> Vec2f -> IO ()
drawRectBoundedBy (Vec2 left top) (Vec2 right bottom) = renderPrimitive Quads vecs
    where vecs = mapM_ vertexVec corners
          corners = [ Vec2 left top
                    , Vec2 right top
                    , Vec2 right bottom
                    , Vec2 left bottom
                    ]

anchoredRectBounds (Rect pos size) anchorPoint = (topLeft, botRight)
    where topLeft = pos - anchorPoint * size
          botRight = pos + (v1 - anchorPoint) * size
          v1 = Vec2 1 1

drawAnchoredRect rect anchor = drawRectBoundedBy topLeft botRight
    where (topLeft, botRight) = anchoredRectBounds rect anchor

drawRect rect = drawAnchoredRect rect (Vec2 0.5 0.5)
drawTopLeftRect rect = drawAnchoredRect rect (Vec2 0 0)

drawColorRect :: RGB -> Rect -> IO ()
drawColorRect color rect = do
    colorRGB color
    drawRect rect
