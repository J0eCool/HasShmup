module Draw where

import Graphics.Rendering.OpenGL hiding (RGB, Rect)
import Graphics.UI.GLUT hiding (RGB, Rect)

import Math.Rect
import Math.Vec

data RGB = RGB Float Float Float

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

colorRGB :: RGB -> IO ()
colorRGB (RGB r g b) = color3f (realToFrac r) (realToFrac g) (realToFrac b)

vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z

vertexVec :: Vec2f -> IO ()
vertexVec (Vec2 x y) = vertex3f (realToFrac x) (realToFrac y) 0

---------------------------------------

drawRectBoundedBy :: Vec2f -> Vec2f -> IO ()
drawRectBoundedBy (Vec2 left top) (Vec2 right bottom) = renderPrimitive Quads vecs
    where vecs = mapM_ vertexVec corners
          corners = [ Vec2 left top
                    , Vec2 right top
                    , Vec2 right bottom
                    , Vec2 left bottom
                    ]

anchoredRectBounds :: Rect -> Vec2f -> (Vec2f, Vec2f)
anchoredRectBounds (Rect pos size) anchorPoint = (topLeft, botRight)
    where topLeft = pos - anchorPoint * size
          botRight = pos + (v1 - anchorPoint) * size
          v1 = Vec2 1 1

drawAnchoredRect :: Vec2f -> Rect -> IO ()
drawAnchoredRect anchor rect = drawRectBoundedBy topLeft botRight
    where (topLeft, botRight) = anchoredRectBounds rect anchor

drawRect :: Rect -> IO ()
drawRect = drawAnchoredRect (Vec2 0.5 0.5)

drawTopLeftRect :: Rect -> IO ()
drawTopLeftRect = drawAnchoredRect (Vec2 0 0)

drawColorRect :: RGB -> Rect -> IO ()
drawColorRect color rect = do
    colorRGB color
    drawRect rect

---------------------------------------

drawAnchoredString :: Vec2f -> String -> Vec2f -> IO ()
drawAnchoredString (Vec2 ax ay) str (Vec2 x y) = preservingMatrix $ do
    color3f 1 1 1
    let s = 0.0004
    w <- realToFrac <$> stringWidth Roman str
    h <- realToFrac <$> fontHeight Roman
    let x' = realToFrac $ realToFrac x - s * w * realToFrac ax
        y' = realToFrac $ realToFrac y + s * h * realToFrac ay
    translate $ Vector3 x' y' (0 :: GLfloat)
    scale s s (s :: GLfloat)
    renderString Roman str

drawString :: String -> Vec2f -> IO ()
drawString = drawAnchoredString (Vec2 0.5 0.5)

drawTopLeftString :: String -> Vec2f -> IO ()
drawTopLeftString = drawAnchoredString (Vec2 0 0)
