{-# LANGUAGE TemplateHaskell #-}

module Entity where

import Control.Lens

import Draw
import Vec

data EntityType = NoType | PlayerType | BallType | BulletType
    deriving (Eq, Show)

type Identifier = Int

data Entity i = Entity
    { _entityId :: Identifier
    , _entityType :: EntityType
    , _update :: i -> Entity i
    , _entitiesToSpawn :: [Entity i]
    , _shouldRemove :: Bool
    , _pos :: Vec2f
    , _size :: Vec2f
    , _color :: RGB
    }
makeLenses ''Entity

instance Eq (Entity i) where
    a == b = (a ^. entityId) == (b ^. entityId)

instance Show (Entity i) where
    show e = show (e ^. entityType) ++ "_" ++ show (e ^. entityId)

newEntity t = ent
    where ent = Entity 0 t update' entitiesToSpawn' shouldRemove' pos' size' color'
          update' _ = ent
          entitiesToSpawn' = []
          shouldRemove' = False
          pos' = Vec2 0 0
          size' = Vec2 0 0
          color' = RGB 1 1 1

setEntId n = entityId .~ n

updateMulti :: Entity i -> i -> [Entity i]
updateMulti ent input = this ++ spawned
  where this = if ent ^. shouldRemove
               then []
               else [ent ^. update $ input]
        spawned = ent ^. entitiesToSpawn

boundingRect ent = Rect (ent ^. pos) (ent ^. size)
draw ent = drawColorRect (ent ^. color) (boundingRect ent)

---------------------------------------

isOfType :: EntityType -> Entity i -> Bool
isOfType t = (== t) . (^. entityType)

isBall :: Entity i -> Bool
isBall = isOfType BallType

isBullet :: Entity i -> Bool
isBullet = isOfType BulletType

isPlayer :: Entity i -> Bool
isPlayer = isOfType PlayerType
