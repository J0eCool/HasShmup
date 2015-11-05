module Bullet (newBullet, newBulletSpawner) where

import Control.Lens

import Draw
import Entity
import PlayerInput
import World
import Vec

newBulletSpawner pos = ent
    where ent = (newEntity NoType) { update = const ent, entitiesToSpawn = spawn }
          spawn (WInput input _ _) =
            if isShooting input
            then [newBullet pos]
            else []

newBullet :: Vec2f -> WorldEntity
newBullet pos = updateBullet pos removeTimer bullet nullInput
    where bullet = (newEntity BulletType)
          removeTimer = 1.5

updateBullet pos removeTimer bullet (WInput _ world collisions) = bullet'
    where bullet' = bullet
                    { update = updateBullet pos' removeTimer' bullet'
                    , shouldRemove = removeTimer' <= 0 || any isBall collisions
                    , boundingRect = Rect pos' size
                    , draw = drawEnt (RGB 1 1 0) bullet'
                    }
          pos' = pos + dT .* vel
          removeTimer' = removeTimer - dT
          dT = world ^. deltaTime
          vel = Vec2 0 4
          size = Vec2 0.04 0.08
          