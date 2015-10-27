{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Entity where

data EntityType = PlayerType | BallType | BulletType
    deriving (Eq)

isOfType :: (Entity i e) => EntityType -> e -> Bool
isOfType t = (== t) . entityType

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
