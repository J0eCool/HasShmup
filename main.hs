{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Data.IORef
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit

import Ball
import Draw
import Entity
import Player
import PlayerInput
import Vec
import World

main :: IO ()
main = do
    (programName, args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    win <- createWindow "Hi guyes"
    
    t <- realToFrac <$> getPOSIXTime
    let ents =
            [ newBall 0.8 pi 5
            , newBall 0.8 0 10
            , newPlayer (Vec2 0 0)
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
    t <- realToFrac <$> getPOSIXTime
    worldRef $~! worldUpdate input t
    inputRef $~! updateInput

    if shouldQuit input
    then exitSuccess
    else postRedisplay Nothing
