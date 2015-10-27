{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

data Rect = Rect Vec2f Vec2f

display :: IORef World -> DisplayCallback
display worldRef = do
    world <- get worldRef
    clear [ColorBuffer]

    worldDraw world

    swapBuffers

--drawEnt :: (Entity e) => RGB -> e -> IO ()
--drawEnt color ent = do
--    colorRGB color
--    draw ent


drawColorRect color (Rect pos (Vec2 w h)) = do
    colorRGB color
    drawRect w h pos

----------------------------------------

data Ball = Ball
    { ballOffset :: Float
    , ballDist :: Float
    , ballRemTime :: Float
    , ballPos :: Vec2f
    }

instance Entity WorldInput Ball where
    draw ball = drawColorRect (0, 1, 1) (Rect pos (Vec2 0.1 0.1))
        where pos = ballPos ball

    update (_, world) ball = ball { ballPos = pos, ballRemTime = time }
        where pos = (ballDist ball) .* unitVec ang
              ang = world ^. timeSinceStart + ballOffset ball
              time = ballRemTime ball - world ^. deltaTime

    shouldRemove _ ball = ballRemTime ball <= 0

newBall :: Float -> Float -> Float -> Ball
newBall dist offset remTime = Ball offset dist remTime (Vec2 0 0)

isBall :: EntityBox WorldInput -> Bool
isBall (EBox (Ball _ _ _ _)) = True
isBall _ = False

--newBall :: Vec2f -> Float -> Float -> Float -> WorldEntity
--newBall pos dist offset remTime = newEntity pos size
--    & update .~ updateBall dist offset
--    & draw .~ drawEnt color
--    & shouldRemove .~ (\(_, world) _ -> world ^. timeSinceStart > remTime)
--    where size = Vec2 0.1 0.1
--          color = (0, 1, 1)

--updateBall :: Float -> Float -> WorldInput -> WorldEntity -> WorldEntity
--updateBall dist offset (_, world) = pos .~ newPos
--    where newPos = dist .*/ unitVec ang
--          ang = world ^. timeSinceStart + offset

----------------------------------------

--newPlayer :: Vec2f -> WorldEntity
--newPlayer pos = newEntity pos size
--    & update .~ updatePlayer 1
--    & draw .~ drawEnt color
--    & entsToSpawn .~ spawnFromPlayer
--    where size = Vec2 0.2 0.25
--          color = (1, 0, 0)

--updatePlayer :: Float -> WorldInput -> WorldEntity -> WorldEntity
--updatePlayer speed (input, world) = pos %~ (+ delta)
--    where dT = world ^. deltaTime
--          dir = Vec2 (fromIntegral $ xDir input) (negate . fromIntegral $ yDir input)
--          delta = (speed * dT) .*/ dir

--spawnFromPlayer :: WorldInput -> WorldEntity -> [WorldEntity]
--spawnFromPlayer (input, world) player =
--    if isShooting input
--    then [newBullet (player ^. pos) world]
--    else []

----------------------------------------

--newBullet :: Vec2f -> World -> WorldEntity
--newBullet p world = newEntity p size
--    & update .~ (\(_, world) -> pos %~ (+ ((Vec2 0 4) /*. (world ^. deltaTime))))
--    & draw .~ drawEnt (1, 1, 0)
--    & shouldRemove .~ (\(_, w) _ -> w ^. timeSinceStart > endTime)
--    where size = Vec2 0.08 0.12
--          endTime = world ^. timeSinceStart + 1.5

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
            [ newBall 0.8 pi 5
            , newBall 0.8 0 10
            --, newPlayer (Vec2 0 0)
            ]
    worldRef <- newIORef (newWorld t ents)
    inputRef <- newIORef newInput

    displayCallback $= display worldRef
    idleCallback $= Just (updateWorld worldRef inputRef)
    keyboardMouseCallback $= Just (handleInput inputRef)

    mainLoop
