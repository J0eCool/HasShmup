import Data.IORef
import Data.Time.Clock.POSIX
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit

import Math.Vec
import PlayerInput
import World

import Enemy
import Player

getFracTime :: IO Double
getFracTime = realToFrac <$> getPOSIXTime

main :: IO ()
main = do
    (programName, args) <- getArgsAndInitialize
    initialWindowSize $= Size 800 800
    initialWindowPosition $= Position 200 100
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    win <- createWindow "HasShmup"
    
    t <- getFracTime
    let ents =
            [ newPlayer (Vec2 0 0)
            , newEnemySpawner (Vec2 0.0 1.2)
            ]
    worldRef <- newIORef (newWorld t ents)
    inputRef <- newIORef newInput

    displayCallback $= display worldRef
    idleCallback $= Just (updateWorld worldRef inputRef)
    keyboardMouseCallback $= Just (handleInput inputRef)

    mainLoop

display :: IORef World -> DisplayCallback
display worldRef = do
    world <- get worldRef
    clear [ColorBuffer]

    worldDraw world

    swapBuffers

updateWorld :: IORef World -> IORef PlayerInput -> IdleCallback
updateWorld worldRef inputRef = do
    world <- get worldRef
    input <- get inputRef
    t <- getFracTime
    worldRef $~! worldUpdate input t
    inputRef $~! updateInput

    if shouldQuit input
    then exitSuccess
    else postRedisplay Nothing
