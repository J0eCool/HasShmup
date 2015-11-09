module Ball (newBall) where

import Control.Lens

import Draw
import Entity
import PlayerInput
import World
import Vec

newBall :: Vec2f -> WorldEntity
newBall pos' = updateBall ball nullInput
    where ball = newEntity BallType
                 & pos .~ pos'
                 & size .~ Vec2 0.1 0.15
                 & color .~ RGB 0 1 1

updateBall ball (WInput _ world collisions) = ball'
    where ball' = ball
                  & update .~ updateBall ball'
                  & pos +~ deltaPos
                  & shouldRemove .~ (any isBullet collisions || isOffScreen)
          deltaPos = dT .* Vec2 0 (-speed)
          dT = world ^. deltaTime
          (Vec2 x y) = ball ^. pos
          isOffScreen = y < (-0.8)
          speed = 0.15


