{-# LANGUAGE TemplateHaskell #-} 

module Entity (Entity, newEntity, pos, size, update, draw, entsToSpawn, shouldRemove) where

import Control.Lens
import Control.Monad.State.Lazy

import PlayerInput
import Vec

data Entity i = EntityImpl
    { _pos :: Vec2f
    , _size :: Vec2f
    , _update :: i -> Entity i -> Entity i
    , _draw :: Entity i -> IO ()
    , _entsToSpawn :: i -> Entity i -> [Entity i]
    , _shouldRemove :: i -> Entity i -> Bool
    }
makeLenses ''Entity

newEntity pos size = EntityImpl pos size update draw entsToSpawn shouldRemove
    where update _ = id
          draw _ = return ()
          entsToSpawn _ _ = []
          shouldRemove _ _ = False
