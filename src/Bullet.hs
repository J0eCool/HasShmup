module Bullet (newBullet) where

import Control.Lens

import Draw
import Entity
import EntityType
import Math.Vec
import Message
import PlayerInput
import World

newBullet :: Int -> Vec2f -> Vec2f -> WorldEntity
newBullet damage vel pos' = updateBullet damage vel nullInput bullet
    where bullet = newEntity BulletType
                   & pos .~ pos'
                   & size .~ Vec2 0.04 0.08
                   & color .~ RGB 1 1 0
          removeTimer = 1.5

updateBullet :: Int -> Vec2f -> WorldInput -> WorldEntity -> WorldEntity
updateBullet damage vel input bullet = moveRemOffscreen vel input bullet
    & update .~ updateBullet damage vel
    & shouldRemove ||~ (not . null) collisions
    & messagesToSend .~ messages
    where collisions = filter isEnemy . findCollisions bullet $ input ^. worldInput . entities
          messages = map (Message $ DamageMessage damage) collisions
