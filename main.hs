{-# LANGUAGE TemplateHaskell #-} 

import Control.Lens
import Data.IORef
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit

import Draw
import Entity
import PlayerInput
import Vec
import World

display :: IORef World -> DisplayCallback
display worldRef = do
    world <- get worldRef
    clear [ColorBuffer]

    worldDraw world

    swapBuffers

drawEnt :: RGB -> WorldEntity -> IO ()
drawEnt color ent = do
    colorRGB color
    drawRect w h p
    where (Vec2 w h) = ent ^. size
          p = ent ^. pos

----------------------------------------

newBall :: Vec2 -> Float -> Float -> Float -> WorldEntity
newBall pos dist offset remTime = newEntity pos size
    & update .~ updateBall dist offset
    & draw .~ drawEnt color
    & shouldRemove .~ (\(_, world) _ -> world ^. timeSinceStart > remTime)
    where size = Vec2 0.1 0.1
          color = (0, 1, 1)

updateBall :: Float -> Float -> WorldInput -> WorldEntity -> WorldEntity
updateBall dist offset (_, world) = pos .~ newPos
    where newPos = dist .*/ unitVec ang
          ang = world ^. timeSinceStart + offset

----------------------------------------

newPlayer :: Vec2 -> WorldEntity
newPlayer pos = newEntity pos size
    & update .~ updatePlayer 1
    & draw .~ drawEnt color
    & entsToSpawn .~ spawnFromPlayer
    where size = Vec2 0.2 0.25
          color = (1, 0, 0)

updatePlayer :: Float -> WorldInput -> WorldEntity -> WorldEntity
updatePlayer speed (input, world) = pos %~ (/+/ delta)
    where dT = world ^. deltaTime
          dir = Vec2 (fromIntegral $ xDir input) (negate . fromIntegral $ yDir input)
          delta = (speed * dT) .*/ dir

spawnFromPlayer :: WorldInput -> WorldEntity -> [WorldEntity]
spawnFromPlayer (input, world) player =
    if isShooting input
    then [newBullet (player ^. pos) world]
    else []

----------------------------------------

newBullet :: Vec2 -> World -> WorldEntity
newBullet p world = newEntity p size
    & update .~ (\(_, world) -> pos %~ (/+/ ((Vec2 0 4) /*. (world ^. deltaTime))))
    & draw .~ drawEnt (1, 1, 0)
    & shouldRemove .~ (\(_, w) _ -> w ^. timeSinceStart > endTime)
    where size = Vec2 0.08 0.12
          endTime = world ^. timeSinceStart + 1.5

updateWorld :: IORef World -> IORef PlayerInput -> IdleCallback
updateWorld worldRef inputRef = do
    world <- get worldRef
    input <- get inputRef
    t <- realToFrac <$> getPOSIXTime
    worldRef $~! worldUpdate input t
    inputRef $~! updateInput

    if shouldQuit input
    then exitSuccess
    else postRedisplay Nothing

----------------------------------------

main :: IO ()
main = do
    (programName, args) <- getArgsAndInitialize
    --initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    win <- createWindow "Hi guyes"
    
    t <- realToFrac <$> getPOSIXTime
    let ents =
            [ newBall (Vec2 0 0.8) 0.8 pi 5
            , newBall (Vec2 0.8 0) 0.8 0 10
            , newPlayer (Vec2 0 0)
            ]
    worldRef <- newIORef (newWorld t ents)
    inputRef <- newIORef newInput

    displayCallback $= display worldRef
    idleCallback $= Just (updateWorld worldRef inputRef)
    keyboardMouseCallback $= Just (handleInput inputRef)

    mainLoop
