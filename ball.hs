{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ball where

import Control.Lens

import Draw
import Entity
import PlayerInput
import World
import Vec

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

    shouldRemove _ ball = ballRemTime ball <= 0

    boundingRect ball = Rect (ballPos ball) (Vec2 0.1 0.1)

    draw = drawEnt (RGB 0 1 1)

newBall :: Float -> Float -> Float -> WorldEntity
newBall dist offset remTime = EBox $ Ball offset dist remTime (Vec2 0 0)
