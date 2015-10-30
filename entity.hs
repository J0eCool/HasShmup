{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Entity where

import Vec

data EntityType = PlayerType | BallType | BulletType
    deriving (Eq, Show)

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
                   else [eBox $ update input ent]
            spawned = entitiesToSpawn input ent

    handleCollisions :: [EntityBox i] -> e -> e
    handleCollisions _ = id

    boundingRect :: e -> Rect
    boundingRect _ = rect 0 0 0 0

    draw :: e -> IO ()
    draw _ = return ()

data EntityBox i where
    EBox :: (Entity i e) => Int -> e -> EntityBox i

instance Entity i (EntityBox i) where
    entityType (EBox _ e) = entityType e
    update i (EBox x e) = EBox x (update i e)
    entitiesToSpawn i (EBox _ e) = map (EBox 0) . entitiesToSpawn i $ e
    shouldRemove i (EBox _ e) = shouldRemove i e
    updateMulti i (EBox x e) = this ++ spawned
      where this = if shouldRemove i e
                   then []
                   else [EBox x $ update i e]
            spawned = entitiesToSpawn i e
    boundingRect (EBox _ e) = boundingRect e
    draw (EBox _ e) = draw e

eBox ent = EBox 0 ent
setEntId x (EBox _ e) = EBox x e
getEntId (EBox x _) = x

instance Eq (EntityBox i) where
    (EBox x _) == (EBox y _) = x == y

instance Show (EntityBox i) where
    show (EBox x e) = show (entityType e) ++ "_" ++ show x

---------------------------------------

isOfType :: (Entity i e) => EntityType -> e -> Bool
isOfType t = (== t) . entityType

isBall :: (Entity i e) => e -> Bool
isBall = isOfType BallType

isBullet :: (Entity i e) => e -> Bool
isBullet = isOfType BulletType

isPlayer :: (Entity i e) => e -> Bool
isPlayer = isOfType PlayerType
