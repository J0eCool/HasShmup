{-# LANGUAGE RankNTypes #-}

import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import Data.Time
import Graphics.UI.GLUT
import Reactive.Banana
import Reactive.Banana.Frameworks

import Draw
import Vec

main :: IO ()
main = do
    (programName, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Reactive World"
    reactiveGLUT program

data Inputs t = Inputs {
    time :: Behavior t DiffTime
}

data Outputs t = Outputs {
    display :: Behavior t DisplayCallback
}

reactiveGLUT :: (forall t. Inputs t -> Outputs t) -> IO ()
reactiveGLUT program = do
    -- Initial values
    initialTime <- getCurrentTime

    -- Events
    (addTime, raiseTime) <- newAddHandler
    (addDisplay, raiseDisplay) <- newAddHandler

    -- Output vars
    displayVar <- newEmptyMVar
    whenIdleRef <- newIORef $ return ()
    let setDisplay = putMVar displayVar
        runDisplay = takeMVar displayVar >>= id
        addWhenIdle y = atomicModifyIORef' whenIdleRef (\x -> (x >> y, ()))
        runWhenIdle = atomicModifyIORef' whenIdleRef (\x -> (return (), x)) >>= id

        -- Reactive network for GLUT
        networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            clock <- fromChanges initialTime addTime
            displayEvent <- fromAddHandler addDisplay
            let diffTime = realToFrac . (flip diffUTCTime) initialTime <$> clock
                inputs = Inputs diffTime
                outputs = program inputs
                displayPoll = display outputs <@ displayEvent
            reactimate $ fmap setDisplay displayPoll

    network <- compile networkDescription
    actuate network

    -- GLUT callbacks
    idleCallback $= Just (do
        getCurrentTime >>= raiseTime
        runWhenIdle
        postRedisplay Nothing)
    displayCallback $= do
        raiseDisplay ()
        runDisplay
    mainLoop

program :: Inputs t -> Outputs t
program inputs = Outputs
    --{ display = drawWorld <$> angleB inputs
    { display = drawWorld . realToFrac <$> time inputs
    }

--angleB inputs = accumB 0 . (+ time inputs)
--    where

drawWorld :: Float -> DisplayCallback
drawWorld t = do
    clear [ColorBuffer]
    drawRect 0.2 0.2 (0.8 .*/ unitVec t)
    swapBuffers

