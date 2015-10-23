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

data KeyboardMouse = KeyboardMouse
    { key :: Key
    , keyState :: KeyState
    , modifiers :: Modifiers
    , position :: Position
    }

data Inputs t = Inputs
    { time :: Behavior t DiffTime
    , keyboardMouse :: Event t KeyboardMouse
    , pos :: Behavior t Vec2f
    }

data Outputs t = Outputs
    { display :: Behavior t DisplayCallback
    }

reactiveGLUT :: (forall t. Inputs t -> Outputs t) -> IO ()
reactiveGLUT program = do
    -- Initial values
    initialTime <- getCurrentTime

    -- Events
    (addKeyboardMouse, raiseKeyboardMouse) <- newAddHandler
    (addTime, raiseTime) <- newAddHandler
    (addPos, raisePos) <- newAddHandler
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
            keyboardMouseEvent <- fromAddHandler addKeyboardMouse
            clock <- fromChanges initialTime addTime
            displayEvent <- fromAddHandler addDisplay
            let diffTime = realToFrac . (flip diffUTCTime) initialTime <$> clock
                inputs = Inputs diffTime keyboardMouseEvent (pure $ Vec2 0 0)
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
    keyboardMouseCallback $= Just (\k ks m p -> raiseKeyboardMouse $ KeyboardMouse k ks m p)
    displayCallback $= do
        raiseDisplay ()
        runDisplay
    mainLoop

program :: Inputs t -> Outputs t
program inputs = Outputs
    --{ display = drawWorld <$> angleB inputs
    { display = drawWorld <$> (realToFrac <$> time inputs) <*> positionB inputs
    }

positionChange (KeyboardMouse (Char c) Down _ _) = case c of
    'a' -> Just . (+) $ (Vec2 (-dist) 0)
    'd' -> Just . (+) $ (Vec2 dist 0)
    'w' -> Just . (+) $ (Vec2 0 dist)
    's' -> Just . (+) $ (Vec2 0 (-dist))
    _ -> Nothing
    where dist = 0.1
positionChange _ = Nothing

positionB = accumB (Vec2 0 0) . filterJust . fmap positionChange . keyboardMouse

--angleB inputs = accumB 0 . (+ time inputs)
--    where

drawWorld :: Float -> Vec2f -> DisplayCallback
drawWorld t pos = do
    clear [ColorBuffer]

    colorRGB (0, 1, 1)
    drawRect 0.2 0.2 (0.8 .*/ unitVec t)

    colorRGB (1, 0, 0)
    drawRect 0.2 0.3 pos

    swapBuffers
