{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Player where

import Control.Lens

import Bullet
import Draw
import Entity
import PlayerInput
import World
import Vec

data Player = Player
    { playerPos :: Vec2f
    }

instance Entity WorldInput Player where
    entityType _ = PlayerType

    update (input, world) player = player { playerPos = pos }
        where pos = playerPos player + delta
              delta = (speed * dT) .* dir
              speed = 1
              dT = world ^. deltaTime
              dir = Vec2 (fromIntegral $ xDir input) (negate . fromIntegral $ yDir input)

    draw player = drawColorRect (1, 0, 0.5) (Rect (playerPos player) (Vec2 0.1 0.15))

    entitiesToSpawn (input, world) player =
        if isShooting input
        then [newBullet (playerPos player) world]
        else []

newPlayer :: Vec2f -> WorldEntity
newPlayer pos = EBox $ Player pos
