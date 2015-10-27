{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Player (newPlayer) where

import Control.Lens

import Bullet
import Draw
import Entity
import PlayerInput
import World
import Vec

data Player = Player
    { playerPos :: Vec2f
    , shotCooldown :: Float
    }

instance Entity WorldInput Player where
    entityType _ = PlayerType

    update (input, world) player = player { playerPos = pos, shotCooldown = cooldown }
        where pos = playerPos player + delta
              delta = (speed * dT) .* dir
              speed = 1
              dT = world ^. deltaTime
              cooldown = if shouldShoot player input
                then 0.4
                else shotCooldown player - dT
              dir = Vec2 (fromIntegral $ xDir input) (negate . fromIntegral $ yDir input)

    draw player = drawColorRect (1, 0, 0.5) (Rect (playerPos player) (Vec2 0.1 0.15))

    entitiesToSpawn (input, world) player =
        if shouldShoot player input
        then [newBullet (playerPos player) world]
        else []

shouldShoot player input = cooldownElapsed && buttonPressed
    where cooldownElapsed = shotCooldown player <= 0
          buttonPressed = isShooting input

newPlayer :: Vec2f -> WorldEntity
newPlayer pos = EBox $ Player pos 0
