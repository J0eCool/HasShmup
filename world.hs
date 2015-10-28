{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module World (World, WorldInput, WorldEntity, newWorld, addEntity,
    worldUpdate, worldDraw, timeSinceStart, deltaTime, entities) where

import Control.Lens
import Control.Monad.State.Lazy

import Entity
import PlayerInput
import Vec

type WorldInput = (PlayerInput, World)
type WorldEntity = EntityBox WorldInput

data World = World
    { _lastTimestamp :: Double
    , _timeSinceStart :: Float
    , _deltaTime :: Float
    , _nextEntityId :: Int
    , _entities :: [WorldEntity]
    }
makeLenses ''World

addEntity :: WorldEntity -> World -> World
addEntity ent world = world
    & nextEntityId +~ 1
    & entities %~ (ent :)

newWorld :: Double -> [WorldEntity] -> World
newWorld t ents = foldr addEntity baseWorld ents
    where baseWorld = World t 0 0 0 []

worldUpdate :: PlayerInput -> Double -> World -> World
worldUpdate input t world = world
    & timeSinceStart +~ dT
    & deltaTime .~ dT
    & lastTimestamp .~ t
    & entities %~ concatMap (updateMulti entInput)
    where dT = realToFrac $ t - oldT
          oldT = world ^. lastTimestamp
          entInput = (input, world)

worldDraw :: World -> IO ()
worldDraw world = mapM_ draw (world ^. entities)
