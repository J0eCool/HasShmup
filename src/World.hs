{-# LANGUAGE TemplateHaskell #-}

module World where

import Control.Lens
import Data.Maybe
import Data.List
import qualified Data.Map as Map

import Entity
import Math.Rect
import Math.Vec
import PlayerInput

import System.Random

data WorldInput = WInput
    { _playerInput :: PlayerInput
    , _worldInput :: World
    , _messageInput :: [EntityMessage]
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
    where baseWorld = World t 0 0 0 [] Map.empty

nullInput :: WorldInput
nullInput = WInput newInput (newWorld 0 []) [] (mkStdGen 0)

worldUpdate :: PlayerInput -> Double -> World -> World
worldUpdate input t =
      worldRemoveEntities
    . worldAddEntities
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
    where ents = world ^. entities
          updateEnt e = (e ^. update) (inputFunc e) e
          inputFunc e = WInput input world msgs rand
            where rand = mkStdGen $ e ^. entityId * 47 + milis
                  milis = floor (t * 1000)
                  t = world ^. timeSinceStart
                  msgs = fromMaybe [] (Map.lookup e (world ^. lastFrameMessages))

worldUpdateMessages world = world
    & lastFrameMessages .~ messageMap
    where messages = concatMap (^. messagesToSend) (world ^. entities)
          addMessage m (MessageSend e msg) = Map.insertWith (++) e [msg] m 
          messageMap = foldl addMessage Map.empty messages

worldRemoveEntities world = world
    & entities %~ filter (not . (^. shouldRemove))

worldAddEntities world = addEntities entsToAdd world
    where entsToAdd = concatMap (^. entitiesToSpawn) (world ^. entities)


worldDraw :: World -> IO ()
worldDraw world = mapM_ (callOnSelf draw) (world ^. entities)
