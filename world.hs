{-# LANGUAGE TemplateHaskell #-} 

module World (World, newWorld, worldUpdate, timeSinceStart, userAngle) where

import Control.Lens
import Control.Monad.State.Lazy

import PlayerInput

data World = World
    { _lastTimestamp :: Double
    , _timeSinceStart :: Float
    , _userAngle :: Float
    }
    deriving Show
makeLenses ''World

newWorld t = World t 0 0

worldUpdate input t world = execState worldState world
    where worldState = do
            oldT <- use lastTimestamp
            let dT = realToFrac $ t - oldT
            lastTimestamp .= t
            timeSinceStart += dT
            userAngle += 3 * dT * fromIntegral (xDir input)


