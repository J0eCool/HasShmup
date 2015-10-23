{-# LANGUAGE TemplateHaskell #-} 

module Entity (Entity, newEntity, entId, pos, size,
  update, draw, entsToSpawn, shouldRemove) where

import Control.Lens
import Control.Monad.State.Lazy

import PlayerInput
import Vec

data Entity i = EntityImpl
    { _entId :: Int
    , _pos :: Vec2f
    , _size :: Vec2f
    , _update :: i -> Entity i -> Entity i
    , _draw :: Entity i -> IO ()
    , _entsToSpawn :: i -> Entity i -> [Entity i]
    , _shouldRemove :: i -> Entity i -> Bool
    }
makeLenses ''Entity

instance Show (Entity i) where
  show e = "<Entity_" ++ show (e ^. entId)
    ++ " Pos=" ++ show (e ^. pos)
    ++ " Size=" ++ show (e ^. size)
    ++ ">"

newEntity pos size = EntityImpl 0 pos size update draw entsToSpawn shouldRemove
    where update _ = id
          draw _ = return ()
          entsToSpawn _ _ = []
          shouldRemove _ _ = False
