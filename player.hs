{-# LANGUAGE TemplateHaskell #-}

module Player (newPlayer) where

import Control.Lens
import Control.Monad

import Bullet
import Draw
import Entity
import Health
import PlayerInput
import Time
import World
import Vec

data PlayerInfo = PlayerInfo
    { _health :: Health
    , _burstTimer :: Float
    , _shotTimer :: Float
    }
    deriving (Eq)
makeLenses ''PlayerInfo

newInfo health = PlayerInfo (newHealth health 1.4) 0 0

updateInfo dT damageTaken shootPressed info = info
    & health %~ updateHealth dT damageTaken
    & burstTimer %~ updateTimer shootPressed dT timePerBurst
    & shotTimer %~ updateTimer (shouldShoot info) dT timePerShot

shouldShoot info = info ^. burstTimer > 0 && info ^. shotTimer <= 0

shotsPerBurst = 2
timePerBurst = 0.15
timePerShot = timePerBurst / shotsPerBurst


newPlayer :: Vec2f -> WorldEntity
newPlayer p = updatePlayer info nullInput player
    where player = newEntity PlayerType
                   & pos .~ p
                   & size .~ Vec2 0.1 0.15
                   & color .~ RGB 1 0 0.5
          info = newInfo health
          health = 3

updatePlayer info (WInput input world collisions) player = player'
    where player' = player
                    & update .~ updatePlayer info'
                    & entitiesToSpawn .~ toSpawn
                    & pos %~ clampPos . (+ deltaPos)
                    & draw .~ drawPlayer info'
                    & shouldRemove .~ isDead (info' ^. health)
          info' = updateInfo dT damageTaken shootPressed info
          deltaPos = (speed * dT) .* dir
          dir = Vec2 (fromIntegral $ xDir input) (negate . fromIntegral $ yDir input)
          dT = world ^. deltaTime
          curPos = player ^. pos
          clampPos = clampVec (Vec2 (-0.95) (-0.875)) (Vec2 0.95 0.15)

          didCollide = any isBall collisions
          damageTaken = if didCollide then 1 else 0
          shootPressed = isShooting input

          toSpawn = if shouldShoot info then [newBullet curPos] else []

          speed = 1

drawPlayer info player = do
    when (isNotFlashing h) $ drawEnt player
    drawHealthBar 0.5 h
    where h = info ^. health
