module Player (newPlayer) where

import Control.Lens

import Bullet
import Draw
import Entity
import PlayerInput
import World
import Vec

shotsPerBurst = 3
timePerBurst = 0.3
timePerShot = timePerBurst / shotsPerBurst

newPlayer :: Vec2f -> WorldEntity
newPlayer pos = updatePlayer pos 0 0 (newEntity PlayerType) nullInput

updatePlayer pos burstTimer shotTimer player (WInput input world _) = player'
    where player' = player
                    { update = updatePlayer pos' burstTimer' shotTimer' player'
                    , entitiesToSpawn = toSpawn
                    , boundingRect = Rect pos' size
                    , draw = drawEnt color player'
                    }
          pos' = pos + deltaPos
          deltaPos = (speed * dT) .* dir
          dir = Vec2 (fromIntegral $ xDir input) (negate . fromIntegral $ yDir input)
          dT = world ^. deltaTime

          shouldShoot = burstTimer > 0 && shotTimer <= 0
          burstTimer' = if isShooting input then timePerBurst else burstTimer - dT
          shotTimer' = if shouldShoot then timePerShot else shotTimer - dT
          toSpawn = if shouldShoot then [newBullet pos'] else []

          speed = 1
          size = Vec2 0.1 0.15
          color = RGB 1 0 0.5
