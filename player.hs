{-# LANGUAGE TemplateHaskell #-}

module Player (newPlayer) where

import Control.Lens

import Bullet
import Draw
import Entity
import PlayerInput
import World
import Vec

data Health = Health
    { _currentHealth :: Int
    , _maxHealth :: Int
    , _invincibleTimer :: Float
    }
makeLenses ''Health

newHealth health = Health health health 0
updateHealth dT didCollide health = health
    & currentHealth -~ (if takeDamage then 1 else 0)
    & invincibleTimer %~ updateTimer takeDamage dT timeForInvincibility
    where takeDamage = didCollide && not (isInvincible health)

isInvincible health = health ^. invincibleTimer > 0
updateTimer shouldReset dT resetTime = if shouldReset
                                       then const resetTime
                                       else flip (-) dT

timeForInvincibility = 1.4

data PlayerInfo = PlayerInfo
    { _health :: Health
    , _burstTimer :: Float
    , _shotTimer :: Float
    }
makeLenses ''PlayerInfo

newInfo health = PlayerInfo (newHealth health) 0 0

updateInfo dT didCollide shootPressed info = info
    & health %~ updateHealth dT didCollide
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
          info' = updateInfo dT didCollide shootPressed info
          deltaPos = (speed * dT) .* dir
          dir = Vec2 (fromIntegral $ xDir input) (negate . fromIntegral $ yDir input)
          dT = world ^. deltaTime
          curPos = player ^. pos
          clampPos = clampVec (Vec2 (-0.95) (-0.875)) (Vec2 0.95 0.15)

          didCollide = any isBall collisions
          shootPressed = isShooting input

          toSpawn = if shouldShoot info then [newBullet curPos] else []

          speed = 1

drawPlayer info player = do
    if shouldDraw then drawEnt player else return ()
    drawHealthBar 0.5 h
    where h = info ^. health
          invin = isInvincible h
          shouldDraw = not invin || isFlashing
          isFlashing = even $ floor (h ^. invincibleTimer * 20) `mod` 2

clamp lo hi = min hi . max lo

drawHealthBar maxWidth health = do
    colorRGB $ RGB 0.5 0 0
    drawTopLeftRect $ rect x y maxWidth height
    colorRGB $ RGB 1 0 0
    drawTopLeftRect $ rect x y width height
    where (x, y) = (-0.9, -0.9)
          height = 0.1
          width = pct * maxWidth
          pct = clamp 0 1 $ f currentHealth / f maxHealth
          f = fromIntegral . (health ^.)

