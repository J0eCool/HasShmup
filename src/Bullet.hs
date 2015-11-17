module Bullet (newBullet) where

import Control.Lens

import Draw
import Entity
import Math.Vec
import PlayerInput
import World

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
    & shouldRemove .~ (removeTimer' <= 0 || length collisions > 0)
    & pos +~ dT .* vel
    & messagesToSend .~ messages
    where removeTimer' = removeTimer - dT
          dT = input ^. worldInput . deltaTime
          vel = Vec2 0 4
          collisions = filter isEnemy . findCollisions bullet $ input ^. worldInput . entities
          messages = map (\e -> MessageSend e (DamageMessage 1)) collisions
