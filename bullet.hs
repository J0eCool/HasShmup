module Bullet (newBullet) where

import Control.Lens

import Draw
import Entity
import PlayerInput
import World
import Vec

newBullet :: Vec2f -> WorldEntity
newBullet pos' = updateBullet removeTimer nullInput bullet
    where bullet = newEntity BulletType
                   & pos .~ pos'
                   & size .~ Vec2 0.04 0.08
                   & color .~ RGB 1 1 0
          removeTimer = 1.5

updateBullet :: Float -> WorldInput -> WorldEntity -> WorldEntity
updateBullet removeTimer input bullet = bullet
    & update .~ updateBullet removeTimer'
    & shouldRemove .~ (removeTimer' <= 0 || any isEnemy (input ^. collisionInput))
    & pos +~ dT .* vel
    where removeTimer' = removeTimer - dT
          dT = input ^. worldInput . deltaTime
          vel = Vec2 0 4
