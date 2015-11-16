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

data EntityMessageSend i = MessageSend (Entity i) EntityMessage
data EntityMessage =
      DamageMessage Int
    | NullMessage

data Entity i = Entity
    { _entityId :: Identifier
    , _entityType :: EntityType
    , _update :: i -> Entity i -> Entity i
    , _entitiesToSpawn :: [Entity i]
    , _messagesToSend :: [EntityMessageSend i]
    , _shouldRemove :: Bool
    , _pos :: Vec2f
    , _size :: Vec2f
    , _color :: RGB
    , _draw :: Entity i -> IO ()
    }
makeLenses ''Entity

runOnEntIds f a b = f (a ^. entityId) (b ^. entityId)

instance Eq (Entity i) where
    (==) = runOnEntIds (==)

instance Ord (Entity i) where
    compare = runOnEntIds compare

instance Show (Entity i) where
    show e = show (e ^. entityType) ++ "_" ++ show (e ^. entityId)

newEntity t = ent
    where ent = Entity idNum t update' entitiesToSpawn' messagesToEmit'
                shouldRemove' pos' size' color' draw'
          idNum = 0
          update' _ _ = ent
          entitiesToSpawn' = []
          messagesToEmit' = []
          shouldRemove' = False
          pos' = Vec2 0 0
          size' = Vec2 0 0
          color' = RGB 1 1 1
          draw' = drawEnt

setEntId n = entityId .~ n

boundingRect ent = Rect (ent ^. pos) (ent ^. size)
drawEnt ent = drawColorRect (ent ^. color) (boundingRect ent)

callOnSelf l e = (e ^. l) e

findCollisions :: Entity i -> [Entity i] -> [Entity i]
findCollisions e = filter (entsCollide e)

entsCollide :: Entity i -> Entity i -> Bool
entsCollide e1 e2 = notSame && hasRect r1 && hasRect r2 && rectsOverlap r1 r2
    where notSame = e1 /= e2
          hasRect (Rect _ (Vec2 w h)) = w > 0 && h > 0
          r1 = boundingRect e1
          r2 = boundingRect e2

---------------------------------------

isOfType :: EntityType -> Entity i -> Bool
isOfType t = (== t) . (^. entityType)

isEnemy :: Entity i -> Bool
isEnemy = isOfType EnemyType

isBullet :: Entity i -> Bool
isBullet = isOfType BulletType

isPlayer :: Entity i -> Bool
isPlayer = isOfType PlayerType

---------------------------------------

messageDamageTotal = sum . map damage
    where damage (DamageMessage d) = d
          damage _ = 0
