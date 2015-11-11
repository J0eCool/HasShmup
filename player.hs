module Player (newPlayer) where

import Control.Lens

import Bullet
import Draw
import Entity
import PlayerInput
import World
import Vec

shotsPerBurst = 2
timePerBurst = 0.15
timePerShot = timePerBurst / shotsPerBurst

newPlayer :: Vec2f -> WorldEntity
newPlayer pos' = updatePlayer 0 0 player nullInput
    where player = newEntity PlayerType
                   & pos .~ pos'
                   & size .~ Vec2 0.1 0.15
                   & color .~ RGB 1 0 0.5

updatePlayer burstTimer shotTimer player (WInput input world _) = player'
    where player' = player
                    & update .~ updatePlayer burstTimer' shotTimer' player'
                    & entitiesToSpawn .~ toSpawn
                    & pos %~ clampPos . (+ deltaPos)
          deltaPos = (speed * dT) .* dir
          dir = Vec2 (fromIntegral $ xDir input) (negate . fromIntegral $ yDir input)
          dT = world ^. deltaTime
          curPos = player ^. pos
          clampPos = clampVec (Vec2 (-0.95) (-0.875)) (Vec2 0.95 0.15)

          shouldShoot = burstTimer > 0 && shotTimer <= 0
          burstTimer' = if isShooting input then timePerBurst else burstTimer - dT
          shotTimer' = if shouldShoot then timePerShot else shotTimer - dT
          toSpawn = if shouldShoot then [newBullet curPos] else []

          speed = 1
