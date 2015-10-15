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

            --entities .= filter (\x -> not . (x ^. shouldRemove) entInput $ x) (world ^. entities)

worldDraw world = mapM_ (\x -> x ^. draw $ x) (world ^. entities)


