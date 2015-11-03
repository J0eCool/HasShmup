{-# LANGUAGE TemplateHaskell #-} 

module PlayerInput (PlayerInput, newInput, handleInput, updateInput,
    shouldQuit, xDir, yDir, isShooting) where

import Control.Lens
import Control.Monad.State.Lazy
import Data.IORef
import Graphics.UI.GLUT hiding (None)

data ButtonState = None | Pressed | Held | Released
    deriving (Show, Eq)

data PlayerInput = PlayerInputImpl
    { _left :: ButtonState
    , _right :: ButtonState
    , _up :: ButtonState
    , _down :: ButtonState
    , _shoot :: ButtonState
    , _quit :: ButtonState
    }
makeLenses ''PlayerInput

inputLenses = [left, right, up, down, shoot, quit]

signal Pressed = 1
signal Held = 1
signal Released = 0
signal None = 0
axis neg pos input = n + p
    where n = negate . signal $ input ^. neg
          p = signal $ input ^. pos

xDir :: PlayerInput -> Int
xDir = axis left right

yDir :: PlayerInput -> Int
yDir = axis up down

isShooting :: PlayerInput -> Bool
isShooting input = input ^. shoot == Pressed

shouldQuit :: PlayerInput -> Bool
shouldQuit input = input ^. quit /= None

newInput :: PlayerInput
newInput = PlayerInputImpl None None None None None None

handleInput :: IORef PlayerInput -> KeyboardMouseCallback
handleInput inputRef key keyState _ _ = setInput $ case key of
    (Char 'a') -> left
    (Char 'd') -> right
    (Char 'w') -> up
    (Char 's') -> down
    (Char 'j') -> shoot
    (Char '\ESC') -> quit
    _ -> ignore
    where btnState = if keyState == Down then Pressed else Released
          setInput lens = inputRef $~! lens .~ btnState
          ignore = lens (\_ -> None) (\x _ -> x)

updateInput :: PlayerInput -> PlayerInput
updateInput input = execState inputState input
    where inputState = mapM_ updateLens inputLenses
          updateLens l = l %= (\b -> case b of
            Pressed -> Held
            Released -> None
            _ -> b
            )
