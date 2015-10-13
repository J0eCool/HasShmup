{-# LANGUAGE TemplateHaskell #-} 

import Control.Lens
import Data.IORef
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import World
import PlayerInput

color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)

data Vec = Vec2 Float Float
    deriving (Eq, Show)

vec (x, y) = Vec2 x y
unitVec t = Vec2 (cos t) (sin t)

flatVecOp op (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (op x1 x2) (op y1 y2)
(/+/) = flatVecOp (+)
(/-/) = flatVecOp (-)
(/*/) = flatVecOp (*)

hoistVecOp op (Vec2 x y) = Vec2 (op x) (op y)
s .*/ v = hoistVecOp (* s) v
v /*. s = hoistVecOp (* s) v

vertexVec (Vec2 x y) = vertex3f (realToFrac x) (realToFrac y) 0

square s pos = renderPrimitive Quads vecs
    where vecs = mapM vertexVec corners
          offsets = [ (s, s)
                    , (-s, s)
                    , (-s, -s)
                    , (s, -s)
                    ]
          vecOffsets = map vec offsets
          corners = map (/+/ pos) vecOffsets

display :: IORef World -> DisplayCallback
display worldRef = do
    world <- get worldRef
    let t = world ^. timeSinceStart
        ang = t + world ^. userAngle

    clear [ColorBuffer]

    color3f 1 0 0
    square 0.2 (Vec2 0 0)

    color3f 0 1 1
    square 0.1 (0.8 .*/ unitVec ang)

    flush

update :: IORef World -> IORef PlayerInput -> IdleCallback
update worldRef inputRef = do
    world <- get worldRef
    input <- get inputRef
    t <- realToFrac <$> getPOSIXTime
    worldRef $~! worldUpdate input t

    postRedisplay Nothing

main = do
    (programName, _) <- getArgsAndInitialize
    win <- createWindow "Hi guyes"
    t <- realToFrac <$> getPOSIXTime
    worldRef <- newIORef (newWorld t)
    inputRef <- newIORef newInput
    displayCallback $= display worldRef
    idleCallback $= Just (update worldRef inputRef)
    keyboardMouseCallback $= Just (handleInput inputRef)
    mainLoop
