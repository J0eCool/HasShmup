{-# LANGUAGE TemplateHaskell #-}

module World where

import Control.Lens
import Data.List

import Entity
import PlayerInput
import Rect
import Vec

import System.Random

data WorldInput = WInput
    { _playerInput :: PlayerInput
    , _worldInput :: World
    , _collisionInput :: [WorldEntity]
    , _randInput :: StdGen
    }

type WorldEntity = Entity WorldInput

data World = World
    { _lastTimestamp :: Double
    , _timeSinceStart :: Float
    , _deltaTime :: Float
    , _nextEntityId :: Identifier
    , _entities :: [WorldEntity]
    }

makeLenses ''World
makeLenses ''WorldInput

addEntity :: WorldEntity -> World -> World
addEntity ent world = world
    & nextEntityId %~ succ
    & entities %~ (ent' :)
    where ent' = setEntId curId ent
          curId = world ^. nextEntityId

newWorld :: Double -> [WorldEntity] -> World
newWorld t ents = foldr addEntity baseWorld ents
    where baseWorld = World t 0 0 0 []

nullInput :: WorldInput
nullInput = WInput newInput (newWorld 0 []) [] (mkStdGen 0)

worldUpdate :: PlayerInput -> Double -> World -> World
worldUpdate input t world = updateNewEnts $ world
    & timeSinceStart +~ dT
    & deltaTime .~ dT
    & lastTimestamp .~ t
    & entities %~ concatMap (\e -> updateMulti e (entInputFunc e))
    where dT = realToFrac $ t - oldT
          oldT = world ^. lastTimestamp
          ents = world ^. entities
          entInputFunc e = WInput input world collisions rand
            where collisions = findCollisions ents e
                  rand = mkStdGen $ e ^. entityId * 47 + milis
                  milis = floor (t * 1000)

updateNewEnts :: World -> World
updateNewEnts world = world
    & entities .~ newEnts ++ doneEnts
    & nextEntityId +~ length missingIdEnts
    where newEnts = zipWith setEntId newIds missingIdEnts
          newIds = [curId..]
          (missingIdEnts, doneEnts) = partition ((== 0) . (^. entityId)) (world ^. entities)
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
worldDraw world = mapM_ (callOnSelf draw) (world ^. entities)
