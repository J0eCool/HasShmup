{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bullet where

import Control.Lens

import Draw
import Entity
import PlayerInput
import World
import Vec

data Bullet = Bullet
    { bulletPos :: Vec2f
    , bulletDelTime :: Float
    }

instance Entity WorldInput Bullet where
    entityType _ = BulletType

    update (WInput _ world _) bullet = bullet { bulletPos = pos }
        where pos = (bulletPos bullet) + Vec2 0 4 *. (world ^. deltaTime)

    shouldRemove (WInput _ world collisions) bullet = t > bulletDelTime bullet || didCollide
        where t = world ^. timeSinceStart
              didCollide = any isBall collisions

    boundingRect bullet = Rect (bulletPos bullet) (Vec2 0.04 0.08)

    draw = drawEnt (RGB 1 1 0)

newBullet :: Vec2f -> World -> WorldEntity
newBullet p world = eBox $ Bullet p delTime
    where delTime = world ^. timeSinceStart + 1.5
