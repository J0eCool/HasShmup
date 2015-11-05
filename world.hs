{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module World (World, WorldEntity, WorldInput(..), newWorld, nullInput, addEntity,
    worldUpdate, worldDraw, timeSinceStart, deltaTime, entities, entsCollide) where

import Control.Lens
import Control.Monad.State.Lazy
import Data.List

import Entity
import PlayerInput
import Vec

type WorldEntity = Entity WorldInput
data WorldInput = WInput
    { playerInput :: PlayerInput
    , worldInput :: World
    , collisionInput :: [WorldEntity]
    }

data World = World
    { _lastTimestamp :: Double
    , _timeSinceStart :: Float
    , _deltaTime :: Float
    , _nextEntityId :: Identifier
    , _entities :: [WorldEntity]
    }
makeLenses ''World

addEntity :: WorldEntity -> World -> World
addEntity ent world = world
    & nextEntityId +~ 1
    & entities %~ (ent' :)
    where ent' = setEntId curId ent
          curId = world ^. nextEntityId

newWorld :: Double -> [WorldEntity] -> World
newWorld t ents = foldr addEntity baseWorld ents
    where baseWorld = World t 0 0 0 []

nullInput = WInput newInput (newWorld 0 []) []

worldUpdate :: PlayerInput -> Double -> World -> World
worldUpdate input t world = updateNewEnts $ world
    & timeSinceStart +~ dT
    & deltaTime .~ dT
    & lastTimestamp .~ t
    & entities %~ concatMap (\e -> updateMulti e (entInputFunc e))
    where dT = realToFrac $ t - oldT
          oldT = world ^. lastTimestamp
          ents = world ^. entities
          entInputFunc e = WInput input world collisions
            where collisions = findCollisions ents e

updateNewEnts :: World -> World
updateNewEnts world = world
    & entities .~ newEnts ++ doneEnts
    & nextEntityId +~ length missingIdEnts
    where newEnts = zipWith setEntId newIds missingIdEnts
          newIds = [curId..]
          (missingIdEnts, doneEnts) = partition ((== 0) . entityId) (world ^. entities)
          curId = world ^. nextEntityId

findCollisions :: [WorldEntity] -> WorldEntity -> [WorldEntity]
findCollisions es e = filter (entsCollide e) es

entsCollide :: WorldEntity -> WorldEntity -> Bool
entsCollide e1 e2 = notSame && hasRect r1 && hasRect r2 && rectsOverlap r1 r2
    where notSame = e1 /= e2
          hasRect (Rect _ (Vec2 w h)) = w > 0 && h > 0
          r1 = boundingRect e1
          r2 = boundingRect e2

worldDraw :: World -> IO ()
worldDraw world = mapM_ draw (world ^. entities)
