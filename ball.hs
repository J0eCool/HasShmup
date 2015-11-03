{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Ball (newBall) where

import Control.Lens

import Draw
import Entity
import PlayerInput
import World
import Vec

data Ball = Ball
    { _pos :: Vec2f
    , _vel :: Vec2f
    }
makeLenses ''Ball

instance Entity WorldInput Ball where
    entityType _ = BallType

    update (WInput _ world _) ball = ball
        & pos +~ delta
        where delta = v *. dT
              v = ball ^. vel
              dT = world ^. deltaTime

    shouldRemove (WInput _ _ collisions) ball = any isBullet collisions

    boundingRect ball = Rect (ball ^. pos) (Vec2 0.1 0.1)

    draw = drawEnt (RGB 0 1 1)

newBall :: Vec2f -> WorldEntity
newBall startPos = eBox $ Ball startPos (Vec2 0 0)
