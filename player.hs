{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Player (newPlayer) where

import Control.Lens

import Bullet
import Draw
import Entity
import PlayerInput
import World
import Vec

data Player = Player
    { _pos :: Vec2f
    , _shotCooldown :: Float
    }
makeLenses ''Player

instance Entity WorldInput Player where
    entityType _ = PlayerType

    update (input, world) player = player
        & pos +~ delta
        & shotCooldown +~ cooldownDelta
        where delta = (speed * dT) .* dir
              speed = 1
              dT = world ^. deltaTime
              cooldownDelta = if shouldShoot player input
                then 0.4
                else (-dT)
              dir = Vec2 (fromIntegral $ xDir input) (negate . fromIntegral $ yDir input)

    entitiesToSpawn (input, world) player =
        if shouldShoot player input
        then [newBullet (player ^. pos) world]
        else []

    boundingRect player = Rect (player ^. pos) (Vec2 0.1 0.15)

    draw = drawEnt (RGB 1 0 0.5)


shouldShoot :: Player -> PlayerInput -> Bool
shouldShoot player input = cooldownElapsed && buttonPressed
    where cooldownElapsed = player ^. shotCooldown <= 0
          buttonPressed = isShooting input

newPlayer :: Vec2f -> WorldEntity
newPlayer pos = EBox $ Player pos 0
