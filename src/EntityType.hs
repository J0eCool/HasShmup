module EntityType where

data EntityType =
      NoType
    | BulletType
    | EnemyType
    | ItemType
    | PlayerType
    deriving (Eq, Show)
