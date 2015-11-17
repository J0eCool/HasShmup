{-# LANGUAGE TemplateHaskell #-}

module Player (newPlayer) where

import Control.Lens
import Control.Monad
import Data.Maybe

import Bullet
import Draw
import Entity
import EntityType
import Health
import Math.Vec
import Message
import PlayerInput
import Time
import World

data PlayerInfo = PlayerInfo
    { _health :: Health
    , _burstTimer :: Float
    , _shotTimer :: Float
    , _bulletLevel :: Int
    }
    deriving (Eq)
makeLenses ''PlayerInfo

newInfo health = PlayerInfo (newHealth health 1.4) 0 0 1

updateInfo dT damageTaken shootPressed upgrades info = info
    & health %~ updateHealth dT damageTaken
    & burstTimer %~ updateTimer shootPressed dT timePerBurst
    & shotTimer %~ updateTimer (shouldShoot info) dT timePerShot
    & bulletLevel +~ upgrades

shouldShoot info = info ^. burstTimer > 0 && info ^. shotTimer <= 0

shotsPerBurst = 2
timePerBurst = 0.15
timePerShot = timePerBurst / shotsPerBurst

newPlayer :: Vec2f -> WorldEntity
newPlayer p = updatePlayer info nullInput (newEntity PlayerType)
    & pos .~ p
    & size .~ Vec2 0.1 0.15
    & color .~ RGB 1 0 0.5
    & entitiesToSpawn .~ [newPlayerHealthBar h]
    & broadcastsToSend .~ [PlayerHealthUpdated h]
    where info = newInfo maxHealth
          h = info ^. health
          maxHealth = 3

updatePlayer info input player = player'
    where player' = player
                    & update .~ updatePlayer info'
                    & entitiesToSpawn .~ toSpawn
                    & pos %~ clampPos . (+ deltaPos)
                    & draw .~ when (isNotFlashing health') . drawEnt
                    & shouldRemove .~ isDead health'
                    & broadcastsToSend .~ broadcasts

          info' = updateInfo dT damageTaken shootPressed upgrades info
          health' = info' ^. health
          deltaPos = (speed * dT) .* dir
          dir = Vec2 (fromIntegral $ xDir pInput) (negate . fromIntegral $ yDir pInput)
          dT = input ^. worldInput . deltaTime
          curPos = player ^. pos
          clampPos = clampVec (Vec2 (-0.95) (-0.925)) (Vec2 0.95 0.15)

          inputMessages = input ^. messageInput
          damageTaken = messageDamageTotal inputMessages
          didCollide = damageTaken > 0
          upgrades = messageCount (boolToInt . isMessageUpgrade) inputMessages

          pInput = input ^. playerInput
          shootPressed = isShooting pInput

          toSpawn = if shouldShoot info then [newBullet damage curPos] else []
          damage = info ^. bulletLevel
          broadcasts = if (info ^. health) /= health'
                       then [PlayerHealthUpdated health']
                       else []

          speed = 1

newPlayerHealthBar h = updateBar h nullInput (newEntity NoType)

updateBar h input bar = bar
    & update .~ updateBar h'
    & draw .~ (\_ -> drawHealthBar 0.5 h')
    where h' = fromMaybe h updatedHealth
          updatedHealth = listToMaybe . mapMaybe toHealth $ input ^. broadcastInput
          toHealth (PlayerHealthUpdated h) = Just h
          toHealth _ = Nothing
