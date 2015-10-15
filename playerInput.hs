{-# LANGUAGE TemplateHaskell #-} 

module PlayerInput (PlayerInput, newInput, handleInput, shouldQuit, xDir, yDir) where

import Control.Lens
import Data.IORef
import Graphics.UI.GLUT
import System.Exit

type ButtonState = Bool

data PlayerInput = PlayerInputImpl
    { _left :: ButtonState
    , _right :: ButtonState
    , _up :: ButtonState
    , _down :: ButtonState
    , _quit :: ButtonState
    }
makeLenses ''PlayerInput

signal b = if b then 1 else 0
axis neg pos input = n + p
    where n = negate . signal $ input ^. neg
          p = signal $ input ^. pos

xDir :: PlayerInput -> Int
xDir = axis left right

yDir :: PlayerInput -> Int
yDir = axis up down

shouldQuit :: PlayerInput -> Bool
shouldQuit input = input ^. quit

newInput :: PlayerInput
newInput = PlayerInputImpl False False False False False

handleInput :: IORef PlayerInput -> KeyboardMouseCallback
handleInput inputRef key keyState _ _ = setInput $ case key of
    (Char 'a') -> left
    (Char 'd') -> right
    (Char 'w') -> up
    (Char 's') -> down
    (Char '\ESC') -> quit
    _ -> ignore
    where isKeyDown = keyState == Down
          setInput lens = inputRef $~! lens .~ isKeyDown
          ignore = lens (\_ -> False) (\x _ -> x)
