module Ball (newBallSpawner) where

import Control.Lens
import Control.Monad

import Draw
import Entity
import Health
import PlayerInput
import Time
import World
import Vec

newBall :: Vec2f -> WorldEntity
newBall pos' = updateBall health 0 nullInput ball
    where ball = newEntity BallType
                 & pos .~ pos'
                 & size .~ Vec2 0.1 0.15
          health = newHealth 5 0

updateBall health gotHitTimer (WInput _ world collisions) ball = ball'
    where ball' = ball
                  & pos +~ deltaPos
                  & update .~ updateBall health' gotHitTimer'
                  & shouldRemove .~ (isDead health || isOffScreen)
                  & color .~ if showHitFlash then RGB 1 0.4 0.4 else RGB 0 1 1
          deltaPos = dT .* Vec2 0 (-speed)
          dT = world ^. deltaTime
          (Vec2 x y) = ball ^. pos
          isOffScreen = y < (-1.2)
          speed = 0.65

          gotHit = any isBullet collisions
          gotHitTimer' = updateTimer gotHit dT 0.05 gotHitTimer
          showHitFlash = gotHitTimer' > 0
          damageTaken = if gotHit then 1 else 0
          health' = updateHealth dT damageTaken health

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
          spawnTime' = updateTimer shouldSpawn dT 0.5 spawnTime
          toSpawn = if shouldSpawn then [newBall $ spawner ^. pos] else []
