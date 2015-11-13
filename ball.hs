module Ball (newBallSpawner) where

import Control.Lens

import Draw
import Entity
import PlayerInput
import World
import Vec

newBall :: Vec2f -> WorldEntity
newBall pos' = updateBall nullInput ball
    where ball = newEntity BallType
                 & pos .~ pos'
                 & size .~ Vec2 0.1 0.15
                 & update .~ updateBall
                 & color .~ RGB 0 1 1

updateBall (WInput _ world collisions) ball = ball'
    where ball' = ball
                  & pos +~ deltaPos
                  & shouldRemove .~ (any isBullet collisions || isOffScreen)
          deltaPos = dT .* Vec2 0 (-speed)
          dT = world ^. deltaTime
          (Vec2 x y) = ball ^. pos
          isOffScreen = y < (-1.2)
          speed = 0.65

newBallSpawner :: Vec2f -> WorldEntity
newBallSpawner p = updateSpawner 0 nullInput spawner
    where spawner = newEntity NoType & pos .~ p

updateSpawner spawnTime (WInput _ world collisions) spawner = spawner'
    where spawner' = spawner
                     & pos . xLens +~ xVel
                     & update .~ updateSpawner spawnTime'
                     & entitiesToSpawn .~ toSpawn
          t = world ^. timeSinceStart
          dT = world ^. deltaTime
          xVel = dT * rate * cos (rate * t)
          rate = 7
          shouldSpawn = spawnTime <= 0
          spawnTime' = if shouldSpawn then 0.5 else spawnTime - dT
          toSpawn = if shouldSpawn then [newBall $ spawner ^. pos] else []
