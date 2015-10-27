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
    where curId = world ^. nextEntityId

newWorld t ents = foldr addEntity baseWorld ents
    where baseWorld = World t 0 0 0 []

selff f l = (\x -> (f (x ^. l)) x)
self = selff id
lensFlipSelf l = flip (selff flip l)
callOnSelf l i = lensFlipSelf l $ i

worldUpdate input t world = execState worldState world
    where worldState = do
            oldT <- use lastTimestamp
            let dT = realToFrac $ t - oldT
                entInput = (input, world)
                inputSelf l = callOnSelf l entInput
            deltaTime .= dT
            lastTimestamp .= t
            timeSinceStart += dT
            entities %= filter (not . shouldRemove entInput)
                  . (\es -> es ++ concatMap (entitiesToSpawn entInput) es)
                  . map (update entInput)
            --entities %= filter (not . inputSelf shouldRemove)
            --    . (\es -> es ++ concatMap (inputSelf entitiesToSpawn) es)
            --    . map (inputSelf update)

worldDraw world = mapM_ draw (world ^. entities)


