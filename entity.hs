{-# LANGUAGE TemplateHaskell #-} 

module Entity (Entity, newEntity, pos, size, update, draw) where

import Control.Lens
import Control.Monad.State.Lazy

import PlayerInput
import Vec

data Entity i = EntityImpl
    { _pos :: Vec2
    , _size :: Vec2
    , _update :: i -> Entity i -> Entity i
    , _draw :: Entity i -> IO ()
    }
makeLenses ''Entity

newEntity pos size update draw = EntityImpl pos size update draw
