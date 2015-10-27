{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Entity where

data EntityType = PlayerType | BallType | BulletType
    deriving (Eq)

class Entity i e | e -> i where
    entityType :: e -> EntityType
    update :: i -> e -> e
    draw :: e -> IO ()
    entitiesToSpawn :: i -> e -> [EntityBox i]
    shouldRemove :: i -> e -> Bool

    update _ = id
    draw _ = return ()
    entitiesToSpawn _ _ = []
    shouldRemove _ _ = False

data EntityBox i where
    EBox :: (Entity i e) => e -> EntityBox i

instance Entity i (EntityBox i) where
    update i (EBox e) = EBox (update i e)
    draw (EBox e) = draw e
    entitiesToSpawn i (EBox e) = map EBox . entitiesToSpawn i $ e
    shouldRemove i (EBox e) = shouldRemove i e
    entityType (EBox e) = entityType e

---------------------------------------

isOfType :: (Entity i e) => EntityType -> e -> Bool
isOfType t = (== t) . entityType

isBall :: (Entity i e) => e -> Bool
isBall = isOfType BallType

isBullet :: (Entity i e) => e -> Bool
isBullet = isOfType BulletType

isPlayer :: (Entity i e) => e -> Bool
isPlayer = isOfType PlayerType
