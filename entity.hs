{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Entity where

import Vec

data EntityType = PlayerType | BallType | BulletType
    deriving (Eq)

class Entity i e | e -> i where
    entityType :: e -> EntityType

    update :: i -> e -> e
    update _ = id

    entitiesToSpawn :: i -> e -> [EntityBox i]
    entitiesToSpawn _ _ = []

    shouldRemove :: i -> e -> Bool
    shouldRemove _ _ = False

    updateMulti :: i -> e -> [EntityBox i]
    updateMulti input ent = this ++ spawned
      where this = if shouldRemove input ent
                   then []
                   else [EBox $ update input ent]
            spawned = entitiesToSpawn input ent

    boundingRect :: e -> Rect
    boundingRect _ = rect 0 0 0 0

    draw :: e -> IO ()
    draw _ = return ()

data EntityBox i where
    EBox :: (Entity i e) => e -> EntityBox i

instance Entity i (EntityBox i) where
    entityType (EBox e) = entityType e
    update i (EBox e) = EBox (update i e)
    draw (EBox e) = draw e
    entitiesToSpawn i (EBox e) = map EBox . entitiesToSpawn i $ e
    shouldRemove i (EBox e) = shouldRemove i e

---------------------------------------

isOfType :: (Entity i e) => EntityType -> e -> Bool
isOfType t = (== t) . entityType

isBall :: (Entity i e) => e -> Bool
isBall = isOfType BallType

isBullet :: (Entity i e) => e -> Bool
isBullet = isOfType BulletType

isPlayer :: (Entity i e) => e -> Bool
isPlayer = isOfType PlayerType
