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
    , bulletDidCollide :: Bool
    }

instance Entity WorldInput Bullet where
    entityType _ = BulletType

    update (_, world) bullet = bullet { bulletPos = pos }
        where pos = (bulletPos bullet) + Vec2 0 4 *. (world ^. deltaTime)

    shouldRemove (_, world) bullet = t > bulletDelTime bullet || bulletDidCollide bullet
        where t = world ^. timeSinceStart

    boundingRect bullet = Rect (bulletPos bullet) (Vec2 0.04 0.08)

    draw = drawEnt (RGB 1 1 0)

newBullet :: Vec2f -> World -> WorldEntity
newBullet p world = EBox $ Bullet p delTime False
    where delTime = world ^. timeSinceStart + 1.5
