module Entity where

import Draw
import Vec

data EntityType = NoType | PlayerType | BallType | BulletType
    deriving (Eq, Show)

type Identifier = Int

data Entity i = Entity
    { entityId :: Identifier
    , entityType :: EntityType
    , update :: i -> Entity i
    , entitiesToSpawn :: [Entity i]
    , shouldRemove :: Bool
    , pos :: Vec2f
    , size :: Vec2f
    , color :: RGB
    }

instance Eq (Entity i) where
    a == b = (entityId a) == (entityId b)

instance Show (Entity i) where
    show e = show (entityType e) ++ "_" ++ show (entityId e)

newEntity t = ent
    where ent = Entity 0 t update' entitiesToSpawn' shouldRemove' pos' size' color'
          update' _ = ent
          entitiesToSpawn' = []
          shouldRemove' = False
          pos' = Vec2 0 0
          size' = Vec2 0 0
          color' = RGB 1 1 1

setEntId n ent = ent { entityId = n }

updateMulti :: Entity i -> i -> [Entity i]
updateMulti ent input = this ++ spawned
  where this = if shouldRemove ent
               then []
               else [update ent input]
        spawned = entitiesToSpawn ent

boundingRect ent = Rect (pos ent) (size ent)
draw ent = drawColorRect (color ent) (boundingRect ent)

---------------------------------------

isOfType :: EntityType -> Entity i -> Bool
isOfType t = (== t) . entityType

isBall :: Entity i -> Bool
isBall = isOfType BallType

isBullet :: Entity i -> Bool
isBullet = isOfType BulletType

isPlayer :: Entity i -> Bool
isPlayer = isOfType PlayerType
