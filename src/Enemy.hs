module Enemy (newEnemySpawner) where

import Control.Lens
import Control.Monad

import Draw
import Entity
import EntityType
import Health
import Math.Vec
import Message
import PlayerInput
import Time
import World

newEnemy :: Vec2f -> WorldEntity
newEnemy pos' = updateEnemy health 0 nullInput enemy
    where enemy = newEntity EnemyType
                 & pos .~ pos'
                 & size .~ Vec2 0.1 0.15
          health = newHealth 5 0.01

updateEnemy health gotHitTimer input enemy = moveRemOffscreen (Vec2 0 (-0.35)) input enemy'
    where enemy' = enemy
                  & update .~ updateEnemy health' gotHitTimer'
                  & shouldRemove ||~ isDead health
                  & color .~ (if showHitFlash then RGB 1 0.4 0.4 else RGB 0 1 1)
                  & entitiesToSpawn .~ toSpawn
                  & messagesToSend .~ messages
          curPos = enemy ^. pos
          dT = input ^. worldInput . deltaTime

          gotHit = damageTaken > 0
          damageTaken = messageDamageTotal (input ^. messageInput)
          gotHitTimer' = updateTimer gotHit dT 0.05 gotHitTimer
          showHitFlash = gotHitTimer' > 0
          health' = updateHealth dT damageTaken health

          collisions = filter isPlayer . findCollisions enemy $ input ^. worldInput . entities
          messages = map (Message $ DamageMessage 1) collisions

          toSpawn = if isDead health && didRollUpgrade then [newItem curPos] else []
          didRollUpgrade = randomRoll 8.5 (input ^. randInput)

newItem p = updateItem nullInput (newEntity ItemType)
    & pos .~ p
    & size .~ Vec2 0.08 0.08
    & color .~ RGB 0 0.8 0
    & update .~ updateItem

updateItem input item = moveRemOffscreen (Vec2 0 (-0.25)) input item
    & shouldRemove ||~ didCollide
    & messagesToSend .~ messages
    where collisions = filter isPlayer . findCollisions item $ input ^. worldInput . entities
          messages = map (Message UpgradeMessage) collisions
          didCollide = not . null $ collisions

newEnemySpawner :: Vec2f -> WorldEntity
newEnemySpawner p = updateSpawner 0 nullInput spawner
    where spawner = newEntity NoType & pos .~ p

updateSpawner spawnTime input spawner = spawner'
    where spawner' = spawner
                     & pos . xLens +~ xVel
                     & update .~ updateSpawner spawnTime'
                     & entitiesToSpawn .~ toSpawn
          t = input ^. worldInput . timeSinceStart
          dT = input ^. worldInput . deltaTime
          xVel = dT * rate * cos (rate * t)
          rate = 7
          shouldSpawn = spawnTime <= 0
          spawnTime' = updateTimer shouldSpawn dT 0.5 spawnTime
          toSpawn = if shouldSpawn then [newEnemy $ spawner ^. pos] else []
