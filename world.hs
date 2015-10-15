{-# LANGUAGE TemplateHaskell #-} 

module World (World, WorldInput, WorldEntity, newWorld,
    worldUpdate, worldDraw, timeSinceStart, deltaTime) where

import Control.Lens
import Control.Monad.State.Lazy

import Entity
import PlayerInput

type WorldInput = (PlayerInput, World)
type WorldEntity = Entity WorldInput

data World = World
    { _lastTimestamp :: Double
    , _timeSinceStart :: Float
    , _deltaTime :: Float
    , _entities :: [WorldEntity]
    }
makeLenses ''World

newWorld t ents = World t 0 0 ents

worldUpdate input t world = execState worldState world
    where worldState = do
            oldT <- use lastTimestamp
            let dT = realToFrac $ t - oldT
            deltaTime .= dT
            lastTimestamp .= t
            timeSinceStart += dT
            entities .= map (\x -> (x ^. update) (input, world) x) (world ^. entities)

worldDraw world = mapM_ (\x -> x ^. draw $ x) (world ^. entities)


