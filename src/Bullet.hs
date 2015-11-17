module Bullet (newBullet) where

import Control.Lens

import Draw
import Entity
import EntityType
import Math.Vec
import Message
import PlayerInput
import World

newBullet :: Int -> Vec2f -> WorldEntity
newBullet damage pos' = updateBullet damage nullInput bullet
    where bullet = newEntity BulletType
                   & pos .~ pos'
                   & size .~ Vec2 0.04 0.08
                   & color .~ RGB 1 1 0
          removeTimer = 1.5

updateBullet :: Int -> WorldInput -> WorldEntity -> WorldEntity
updateBullet damage input bullet = moveRemOffscreen (Vec2 0 4) input bullet
    & update .~ updateBullet damage
    & shouldRemove ||~ (not . null) collisions
    & messagesToSend .~ messages
    where collisions = filter isEnemy . findCollisions bullet $ input ^. worldInput . entities
          messages = map (Message $ DamageMessage damage) collisions
