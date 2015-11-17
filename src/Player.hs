{-# LANGUAGE TemplateHaskell #-}

module Player (newPlayer) where

import Control.Lens
import Control.Monad

import Bullet
import Draw
import Entity
import Health
import Math.Vec
import PlayerInput
import Time
import World

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

updatePlayer info input player = player'
    where player' = player
                    & update .~ updatePlayer info'
                    & entitiesToSpawn .~ toSpawn
                    & pos %~ clampPos . (+ deltaPos)
                    & draw .~ drawPlayer info'
                    & shouldRemove .~ isDead (info' ^. health)

          pInput = input ^. playerInput
          info' = updateInfo dT damageTaken shootPressed info
          deltaPos = (speed * dT) .* dir
          dir = Vec2 (fromIntegral $ xDir pInput) (negate . fromIntegral $ yDir pInput)
          dT = input ^. worldInput . deltaTime
          curPos = player ^. pos
          clampPos = clampVec (Vec2 (-0.95) (-0.925)) (Vec2 0.95 0.15)

          didCollide = damageTaken > 0
          damageTaken = messageDamageTotal (input ^. messageInput)
          shootPressed = isShooting pInput

          toSpawn = if shouldShoot info then [newBullet curPos] else []

          speed = 1

drawPlayer info player = do
    when (isNotFlashing h) $ drawEnt player
    drawHealthBar 0.5 h
    where h = info ^. health