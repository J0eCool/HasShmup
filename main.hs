{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

data Rect = Rect Vec2f Vec2f

display :: IORef World -> DisplayCallback
display worldRef = do
    world <- get worldRef
    clear [ColorBuffer]

    worldDraw world

    swapBuffers

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
    entityType _ = BallType

    update (_, world) ball = ball { ballPos = pos, ballRemTime = time }
        where pos = (ballDist ball) .* unitVec ang
              ang = world ^. timeSinceStart + ballOffset ball
              time = ballRemTime ball - world ^. deltaTime

    draw ball = drawColorRect (0, 1, 1) (Rect pos (Vec2 0.1 0.1))
        where pos = ballPos ball

    shouldRemove _ ball = ballRemTime ball <= 0

newBall :: Float -> Float -> Float -> WorldEntity
newBall dist offset remTime = EBox $ Ball offset dist remTime (Vec2 0 0)

isBall :: (Entity i e) => e -> Bool
isBall = isOfType BallType

----------------------------------------

data Player = Player
    { playerPos :: Vec2f
    }

instance Entity WorldInput Player where
    entityType _ = PlayerType

    update (input, world) player = player { playerPos = pos }
        where pos = playerPos player + delta
              delta = (speed * dT) .* dir
              speed = 1
              dT = world ^. deltaTime
              dir = Vec2 (fromIntegral $ xDir input) (negate . fromIntegral $ yDir input)

    draw player = drawColorRect (1, 0, 0.5) (Rect (playerPos player) (Vec2 0.1 0.15))

    entitiesToSpawn (input, world) player =
        if isShooting input
        then [newBullet (playerPos player) world]
        else []

newPlayer :: Vec2f -> WorldEntity
newPlayer pos = EBox $ Player pos

----------------------------------------

data Bullet = Bullet
    { bulletPos :: Vec2f
    , bulletDelTime :: Float
    , bulletDidCollide :: Bool
    }

instance Entity WorldInput Bullet where
    entityType _ = BulletType

    update (_, world) bullet = bullet { bulletPos = pos }
        where pos = (bulletPos bullet) + Vec2 0 4 *. (world ^. deltaTime)

    draw bullet = drawColorRect (1, 1, 0) (Rect (bulletPos bullet) (Vec2 0.04 0.08))

    shouldRemove (_, world) bullet = t > bulletDelTime bullet || bulletDidCollide bullet
        where t = world ^. timeSinceStart

newBullet :: Vec2f -> World -> WorldEntity
newBullet p world = EBox $ Bullet p delTime False
    where delTime = world ^. timeSinceStart + 1.5

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
            , newPlayer (Vec2 0 0)
            ]
    worldRef <- newIORef (newWorld t ents)
    inputRef <- newIORef newInput

    displayCallback $= display worldRef
    idleCallback $= Just (updateWorld worldRef inputRef)
    keyboardMouseCallback $= Just (handleInput inputRef)

    mainLoop
