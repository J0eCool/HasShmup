{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Bullet (newBullet) where

import Control.Lens

import Draw
import Entity
import World
import Vec

data Bullet = Bullet
    { _pos :: Vec2f
    , _vel :: Vec2f
    , _removeTime :: Float
    }
makeLenses ''Bullet


instance Entity WorldInput Bullet where
    entityType _ = BulletType

    update (WInput _ world _) bullet = bullet
        & pos +~ delta
        where delta = v *. dT
              v = bullet ^. vel
              dT = world ^. deltaTime

    shouldRemove (WInput _ world collisions) bullet = t > (bullet ^. removeTime) || didCollide
        where t = world ^. timeSinceStart
              didCollide = any isBall collisions

    boundingRect bullet = Rect (bullet ^. pos) (Vec2 0.04 0.08)

    draw = drawEnt (RGB 1 1 0)

newBullet :: Vec2f -> World -> WorldEntity
newBullet p world = eBox $ Bullet p (Vec2 0 4) removeTime
    where removeTime = world ^. timeSinceStart + 1.5
