{-# LANGUAGE TemplateHaskell #-}

module Entity where

import Control.Lens

import Draw
import Rect
import Vec

data EntityType =
      NoType
    | PlayerType
    | EnemyType
    | BulletType
    deriving (Eq, Show)

type Identifier = Int

data Entity i = Entity
    { _entityId :: Identifier
    , _entityType :: EntityType
    , _update :: i -> Entity i -> Entity i
    , _entitiesToSpawn :: [Entity i]
    , _shouldRemove :: Bool
    , _pos :: Vec2f
    , _size :: Vec2f
    , _color :: RGB
    , _draw :: Entity i -> IO ()
    }
makeLenses ''Entity

instance Eq (Entity i) where
    a == b = (a ^. entityId) == (b ^. entityId)

instance Show (Entity i) where
    show e = show (e ^. entityType) ++ "_" ++ show (e ^. entityId)

newEntity t = ent
    where ent = Entity 0 t update' entitiesToSpawn' shouldRemove' pos' size' color' draw'
          update' _ _ = ent
          entitiesToSpawn' = []
          shouldRemove' = False
          pos' = Vec2 0 0
          size' = Vec2 0 0
          color' = RGB 1 1 1
          draw' = drawEnt

setEntId n = entityId .~ n

boundingRect ent = Rect (ent ^. pos) (ent ^. size)
drawEnt ent = drawColorRect (ent ^. color) (boundingRect ent)

callOnSelf l e = (e ^. l) e

---------------------------------------

isOfType :: EntityType -> Entity i -> Bool
isOfType t = (== t) . (^. entityType)

isEnemy :: Entity i -> Bool
isEnemy = isOfType EnemyType

isBullet :: Entity i -> Bool
isBullet = isOfType BulletType

isPlayer :: Entity i -> Bool
isPlayer = isOfType PlayerType
