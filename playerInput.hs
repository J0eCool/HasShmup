{-# LANGUAGE TemplateHaskell #-} 

module PlayerInput (PlayerInput, handleInput, newInput, xDir, yDir) where

import Control.Lens
import Data.IORef
import Graphics.UI.GLUT

type ButtonState = Bool

data PlayerInput = PlayerInput
    { _left :: ButtonState
    , _right :: ButtonState
    , _up :: ButtonState
    , _down :: ButtonState
    }
makeLenses ''PlayerInput

signal b = if b then 1 else 0
axis neg pos input = n + p
    where n = negate . signal $ input ^. neg
          p = signal $ input ^. pos
xDir = axis left right
yDir = axis up down

newInput = PlayerInput False False False False

handleInput :: IORef PlayerInput -> KeyboardMouseCallback
handleInput inputRef key keyState _ _ = case key of
    (Char 'a') -> inputRef $~! left .~ isKeyDown
    (Char 'd') -> inputRef $~! right .~ isKeyDown
    _ -> return ()
    where isKeyDown = keyState == Down
