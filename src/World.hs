{-# LANGUAGE TemplateHaskell #-}

module World where

import Control.Lens
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import System.Random

import Entity
import EntityType
import Math.Rect
import Math.Vec
import Message
import PlayerInput

data WorldInput = WInput
    { _playerInput :: PlayerInput
    , _worldInput :: World
    , _messageInput :: [EntityMessage]
    , _broadcastInput :: [BroadcastMessage]
    , _randInput :: StdGen
    }

type WorldEntity = Entity WorldInput

data World = World
    { _lastTimestamp :: Double
    , _timeSinceStart :: Float
    , _deltaTime :: Float
    , _nextEntityId :: Identifier
    , _entities :: [WorldEntity]
    , _lastFrameMessages :: Map.Map WorldEntity [EntityMessage]
    , _lastFrameBroadcasts :: [BroadcastMessage]
    }

makeLenses ''World
makeLenses ''WorldInput

addEntity :: WorldEntity -> World -> World
addEntity ent world = world
    & nextEntityId %~ succ
    & entities %~ (ent' :)
    where ent' = setEntId curId ent
          curId = world ^. nextEntityId

addEntities :: [WorldEntity] -> World -> World
addEntities ents world = foldr addEntity world ents

newWorld :: Double -> [WorldEntity] -> World
newWorld t ents = addEntities ents baseWorld
    where baseWorld = World t 0 0 0 [] Map.empty []

nullInput :: WorldInput
nullInput = WInput newInput (newWorld 0 []) [] [] (mkStdGen 0)

worldUpdate :: PlayerInput -> Double -> World -> World
worldUpdate input t =
      worldRemoveEntities
    . worldUpdateMessages
    . worldUpdateEntities input
    . worldUpdateTime t

worldUpdateTime t world = world
    & timeSinceStart +~ dT
    & deltaTime .~ dT
    & lastTimestamp .~ t
    where dT = realToFrac $ t - oldT
          oldT = world ^. lastTimestamp

worldUpdateEntities input world = world
    & entities %~ map updateEnt
    & addEntities entsToAdd
    where ents = world ^. entities
          updateEnt e = (e ^. update) (inputFunc e) e
          inputFunc e = WInput input world msgs broadcasts rand
            where rand = mkStdGen $ e ^. entityId * 29847 + milis * 1789602
                  milis = floor (t * 1000)
                  t = world ^. timeSinceStart
                  msgs = fromMaybe [] (Map.lookup e (world ^. lastFrameMessages))
                  broadcasts = world ^. lastFrameBroadcasts
          entsToAdd = concatMap (^. entitiesToSpawn) (world ^. entities)

worldUpdateMessages world = world
    & lastFrameMessages .~ messageMap
    & lastFrameBroadcasts .~ broadcasts
    where messages = concatMap (^. messagesToSend) ents
          addMessage m (Message msg e) = Map.insertWith (++) e [msg] m 
          messageMap = foldl addMessage Map.empty messages
          broadcasts = concatMap (^. broadcastsToSend) ents
          ents = world ^. entities

worldRemoveEntities world = world
    & entities .~ remaining
    & addEntities addedByRemoved
    where (removed, remaining) = partition (^. shouldRemove) ents
          addedByRemoved = concatMap (^. entitiesToSpawn) removed
          ents = world ^. entities

worldDraw :: World -> IO ()
worldDraw world = mapM_ (callOnSelf draw) (world ^. entities)

------------------------

randomRoll :: Float -> StdGen -> Bool
randomRoll prob = (<= prob) . fst . randomR (1, 100)

moveRemOffscreen vel input ent = ent
    & pos +~ deltaPos
    & shouldRemove ||~ isOffScreen
    where deltaPos = dT .* vel
          dT = input ^. worldInput . deltaTime
          Vec2 _ y = ent ^. pos
          isOffScreen = y < (-1.5) || y > 1.5
