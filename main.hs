{-# LANGUAGE TemplateHaskell #-} 

import Control.Lens
import Data.IORef
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit

import Entity
import PlayerInput
import Vec
import World

color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)

vertexVec (Vec2 x y) = vertex3f (realToFrac x) (realToFrac y) 0

type RGB = (Float, Float, Float)

colorRGB (r, g, b) = color3f (realToFrac r) (realToFrac g) (realToFrac b)

drawRect w h pos = renderPrimitive Quads vecs
    where vecs = mapM_ vertexVec corners
          offsets = [ (w, h)
                    , (-w, h)
                    , (-w, -h)
                    , (w, -h)
                    ]
          vecOffsets = map vec2 offsets
          corners = map (/+/ pos) vecOffsets
drawSquare size = drawRect size size

display :: IORef World -> DisplayCallback
display worldRef = do
    world <- get worldRef
    clear [ColorBuffer]

    worldDraw world

    --color3f 1 0 0
    --square 0.2 (Vec2 0 0)

    --color3f 0 1 1
    --square 0.1 (0.8 .*/ unitVec ang)

    swapBuffers

updateWorld :: IORef World -> IORef PlayerInput -> IdleCallback
updateWorld worldRef inputRef = do
    world <- get worldRef
    input <- get inputRef
    t <- realToFrac <$> getPOSIXTime
    worldRef $~! worldUpdate input t

    if shouldQuit input
    then exitSuccess
    else postRedisplay Nothing

drawEnt :: RGB -> WorldEntity -> IO ()
drawEnt color ent = do
    colorRGB color
    drawRect w h p
    where (Vec2 w h) = ent ^. size
          p = ent ^. pos

updateBall :: Float -> Float -> WorldInput -> WorldEntity -> WorldEntity
updateBall dist offset (_, world) = pos .~ newPos
    where newPos = dist .*/ unitVec ang
          ang = world ^. timeSinceStart + offset

newBall :: Vec2 -> Float -> Float -> WorldEntity
newBall pos dist offset = newEntity pos size
    & update .~ updateBall dist offset
    & draw .~ drawEnt color
    where size = Vec2 0.1 0.1
          color = (0, 1, 1)

newPlayer :: Vec2 -> WorldEntity
newPlayer pos = newEntity pos size
    & update .~ updatePlayer 1
    & draw .~ drawEnt color
    where size = Vec2 0.2 0.25
          color = (1, 0, 0)

updatePlayer :: Float -> WorldInput -> WorldEntity -> WorldEntity
updatePlayer speed (input, world) = pos %~ (/+/ delta)
    where dT = world ^. deltaTime
          dir = Vec2 (fromIntegral $ xDir input) (negate . fromIntegral $ yDir input)
          delta = (speed * dT) .*/ dir

main :: IO ()
main = do
    (programName, args) <- getArgsAndInitialize
    --initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    win <- createWindow "Hi guyes"
    t <- realToFrac <$> getPOSIXTime
    let ents =
            [ newBall (Vec2 0 0.8) 0.8 pi
            , newPlayer (Vec2 0 0)
            , newBall (Vec2 0 0) 0.8 0
            ]
    worldRef <- newIORef (newWorld t ents)
    inputRef <- newIORef newInput
    displayCallback $= display worldRef
    idleCallback $= Just (updateWorld worldRef inputRef)
    keyboardMouseCallback $= Just (handleInput inputRef)
    mainLoop
