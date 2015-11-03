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
    , _shotBurstEnd :: Float
    , _shotCooldown :: Float
    }
makeLenses ''Player

instance Entity WorldInput Player where
    entityType _ = PlayerType

    update (WInput input world _) player = player
        & pos +~ deltaPos
        & shotBurstEnd %~ (if isShooting input
                           then const (t + burstTime)
                           else id)
        & shotCooldown %~ (if shouldShoot player world
                           then const timePerShot
                           else max 0 . (flip (-)) dT)
        where deltaPos = (speed * dT) .* dir
              speed = 1
              dir = Vec2 (fromIntegral $ xDir input) (negate . fromIntegral $ yDir input)
              dT = world ^. deltaTime
              t = world ^. timeSinceStart

    entitiesToSpawn (WInput input world _) player =
        if shouldShoot player world
        then [newBullet (player ^. pos) world]
        else []

    boundingRect player = Rect (player ^. pos) (Vec2 0.1 0.15)

    draw = drawEnt (RGB 1 0 0.5)

shotsPerBurst = 3
burstTime = 0.3
timePerShot = burstTime / shotsPerBurst

shouldShoot :: Player -> World -> Bool
shouldShoot player world = cooldownElapsed && buttonPressed
    where cooldownElapsed = player ^. shotCooldown <= 0
          buttonPressed = player ^. shotBurstEnd >= world ^. timeSinceStart

newPlayer :: Vec2f -> WorldEntity
newPlayer pos = eBox $ Player pos 0 0
