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
    , _nextEntityId :: Int
    , _entities :: [WorldEntity]
    }
    deriving (Show)
makeLenses ''World

addEntity ent world = world
    & nextEntityId +~ 1
    & entities %~ (ent' :)
    where ent' = ent & entId .~ curId
          curId = world ^. nextEntityId

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
            entities %= filter (not . inputSelf shouldRemove)
                . (\es -> es ++ concatMap (inputSelf entsToSpawn) es)
                . map (inputSelf update)

worldDraw world = mapM_ (\x -> x ^. draw $ x) (world ^. entities)


