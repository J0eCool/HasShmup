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
newBullet damage pos' = updateBullet damage removeTimer nullInput bullet
    where bullet = newEntity BulletType
                   & pos .~ pos'
                   & size .~ Vec2 0.04 0.08
                   & color .~ RGB 1 1 0
          removeTimer = 1.5

updateBullet :: Int -> Float -> WorldInput -> WorldEntity -> WorldEntity
updateBullet damage removeTimer input bullet = bullet
    & update .~ updateBullet damage removeTimer'
    & shouldRemove .~ (removeTimer' <= 0 || (not . null) collisions)
    & pos +~ dT .* vel
    & messagesToSend .~ messages
    where removeTimer' = removeTimer - dT
          dT = input ^. worldInput . deltaTime
          vel = Vec2 0 4
          collisions = filter isEnemy . findCollisions bullet $ input ^. worldInput . entities
          messages = map (Message $ DamageMessage damage) collisions
